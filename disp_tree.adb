--  Node displaying (for debugging).
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

--  Display trees in raw form.  Mainly used for debugging.

with Ada.Text_IO; use Ada.Text_IO;
with Name_Table;
with Str_Table;
with Tokens;
with Errorout;
with Files_Map;
with PSL.Dump_Tree;

--  Do not add a use clause for iirs_utils, as it may crash for ill-formed
--  trees, which is annoying while debugging.

package body Disp_Tree is
   --  function Is_Anonymous_Type_Definition (Def : Iir) return Boolean
   --    renames Iirs_Utils.Is_Anonymous_Type_Definition;

   procedure Disp_Iir (N : Iir;
                       Indent : Natural := 1;
                       Flat : Boolean := False);
   procedure Disp_Header (N : Iir);

   procedure Disp_Tree_List_Flat (Tree_List: Iir_List; Tab: Natural);
   pragma Unreferenced (Disp_Tree_List_Flat);

   procedure Put_Indent (Tab: Natural) is
      Blanks : constant String (1 .. 2 * Tab) := (others => ' ');
   begin
      Put (Blanks);
   end Put_Indent;

   procedure Disp_Iir_Number (Node: Iir)
   is
      Res : String (1 .. 10) := "         ]";
      N : Int32 := Int32 (Node);
   begin
      for I in reverse 2 .. 9 loop
         Res (I) := Character'Val (Character'Pos ('0') + (N mod 10));
         N := N / 10;
         if N = 0 then
            Res (I - 1) := '[';
            Put (Res (I - 1 .. Res'Last));
            return;
         end if;
      end loop;
      Put (Res);
   end Disp_Iir_Number;

   -- For iir.

   procedure Disp_Tree_Flat (Tree: Iir; Tab: Natural) is
   begin
      Disp_Iir (Tree, Tab, True);
   end Disp_Tree_Flat;

   procedure Disp_Iir_List
     (Tree_List : Iir_List; Tab : Natural := 0; Flat : Boolean := False)
   is
      El: Iir;
   begin
      if Tree_List = Null_Iir_List then
         Put_Line ("null-list");
      elsif Tree_List = Iir_List_All then
         Put_Line ("list-all");
      elsif Tree_List = Iir_List_Others then
         Put_Line ("list-others");
      else
         New_Line;
         for I in Natural loop
            El := Get_Nth_Element (Tree_List, I);
            exit when El = Null_Iir;
            Put_Indent (Tab);
            Disp_Iir (El, Tab + 1, Flat);
         end loop;
      end if;
   end Disp_Iir_List;

   procedure Disp_Chain
     (Tree_Chain: Iir; Indent: Natural; Flat : Boolean := False)
   is
      El: Iir;
   begin
      New_Line;
      El := Tree_Chain;
      while El /= Null_Iir loop
         Put_Indent (Indent);
         Disp_Iir (El, Indent + 1, Flat);
         El := Get_Chain (El);
      end loop;
   end Disp_Chain;

   procedure Disp_Tree_Flat_Chain (Tree_Chain: Iir; Tab: Natural)
   is
      El: Iir;
   begin
      El := Tree_Chain;
      while El /= Null_Iir loop
         Disp_Iir (El, Tab, True);
         El := Get_Chain (El);
      end loop;
   end Disp_Tree_Flat_Chain;
   pragma Unreferenced (Disp_Tree_Flat_Chain);

   procedure Disp_Tree_List_Flat (Tree_List: Iir_List; Tab: Natural)
   is
      El: Iir;
   begin
      if Tree_List = Null_Iir_List then
         Put_Indent (Tab);
         Put_Line (" null-list");
      elsif Tree_List = Iir_List_All then
         Put_Indent (Tab);
         Put_Line (" list-all");
      elsif Tree_List = Iir_List_Others then
         Put_Indent (Tab);
         Put_Line (" list-others");
      else
         for I in Natural loop
            El := Get_Nth_Element (Tree_List, I);
            exit when El = Null_Iir;
            Disp_Tree_Flat (El, Tab);
         end loop;
      end if;
   end Disp_Tree_List_Flat;

   function Image_Name_Id (Ident: Name_Id) return String
   is
      use Name_Table;
   begin
      if Ident /= Null_Identifier then
         Image (Ident);
         return ''' & Name_Buffer (1 .. Name_Length) & ''';
      else
         return "<anonymous>";
      end if;
   end Image_Name_Id;

   function Image_Iir_Staticness (Static: Iir_Staticness) return String is
   begin
      case Static is
         when Unknown =>
            return "???";
         when None =>
            return "none";
         when Globally =>
            return "global";
         when Locally =>
            return "local";
      end case;
   end Image_Iir_Staticness;

   function Image_Boolean (Bool : Boolean) return String is
   begin
      if Bool then
         return "true";
      else
         return "false";
      end if;
   end Image_Boolean;

   function Image_Iir_Delay_Mechanism (Mech : Iir_Delay_Mechanism)
                                      return String is
   begin
      case Mech is
         when Iir_Inertial_Delay =>
            return "inertial";
         when Iir_Transport_Delay =>
            return "transport";
      end case;
   end Image_Iir_Delay_Mechanism;

   function Image_Iir_Lexical_Layout_Type (V : Iir_Lexical_Layout_Type)
                                          return String is
   begin
      if (V and Iir_Lexical_Has_Mode) /= 0 then
         return " +mode"
           & Image_Iir_Lexical_Layout_Type (V and not Iir_Lexical_Has_Mode);
      elsif (V and Iir_Lexical_Has_Class) /= 0 then
         return " +class"
           & Image_Iir_Lexical_Layout_Type (V and not Iir_Lexical_Has_Class);
      elsif (V and Iir_Lexical_Has_Type) /= 0 then
         return " +type"
           & Image_Iir_Lexical_Layout_Type (V and not Iir_Lexical_Has_Type);
      else
         return "";
      end if;
   end Image_Iir_Lexical_Layout_Type;

   function Image_Iir_Mode (Mode : Iir_Mode) return String is
   begin
      case Mode is
         when Iir_Unknown_Mode =>
            return "???";
         when Iir_Linkage_Mode =>
            return "linkage";
         when Iir_Buffer_Mode =>
            return "buffer";
         when Iir_Out_Mode =>
            return "out";
         when Iir_Inout_Mode =>
            return "inout";
         when Iir_In_Mode =>
            return "in";
      end case;
   end Image_Iir_Mode;

   function Image_Iir_Signal_Kind (Kind : Iir_Signal_Kind) return String is
   begin
      case Kind is
         when Iir_No_Signal_Kind =>
            return "no";
         when Iir_Register_Kind =>
            return "register";
         when Iir_Bus_Kind =>
            return "bus";
      end case;
   end Image_Iir_Signal_Kind;

   function Image_Iir_Pure_State (State : Iir_Pure_State) return String is
   begin
      case State is
         when Pure =>
            return "pure";
         when Impure =>
            return "impure";
         when Maybe_Impure =>
            return "maybe_impure";
         when Unknown =>
            return "unknown";
      end case;
   end Image_Iir_Pure_State;

   function Image_Iir_All_Sensitized (Sig : Iir_All_Sensitized)
                                     return String is
   begin
      case Sig is
         when Unknown =>
            return "???";
         when No_Signal =>
            return "no_signal";
         when Read_Signal =>
            return "read_signal";
         when Invalid_Signal =>
            return "invalid_signal";
      end case;
   end Image_Iir_All_Sensitized;

   function Image_Iir_Constraint (Const : Iir_Constraint) return String is
   begin
      case Const is
         when Unconstrained =>
            return "unconstrained";
         when Partially_Constrained =>
            return "partially constrained";
         when Fully_Constrained =>
            return "fully constrained";
      end case;
   end Image_Iir_Constraint;

   function Image_Date_State_Type (State : Date_State_Type) return String is
   begin
      case State is
         when Date_Extern =>
            return "extern";
         when Date_Disk =>
            return "disk";
         when Date_Parse =>
            return "parse";
         when Date_Analyze =>
            return "analyze";
      end case;
   end Image_Date_State_Type;

   function Image_Tri_State_Type (State : Tri_State_Type) return String is
   begin
      case State is
         when True =>
            return "true";
         when False =>
            return "false";
         when Unknown =>
            return "unknown";
      end case;
   end Image_Tri_State_Type;

   function Image_Time_Stamp_Id (Id : Time_Stamp_Id) return String
     renames Files_Map.Get_Time_Stamp_String;

   function Image_Iir_Predefined_Functions (F : Iir_Predefined_Functions)
                                           return String is
   begin
      return Iir_Predefined_Functions'Image (F);
   end Image_Iir_Predefined_Functions;

   function Image_String_Id (S : String_Id) return String
     renames Str_Table.Image;

   procedure Disp_PSL_Node (N : PSL_Node; Indent : Natural) is
   begin
      Put_Indent (Indent);
      PSL.Dump_Tree.Dump_Tree (N, True);
   end Disp_PSL_Node;

   procedure Disp_PSL_NFA (N : PSL_NFA; Indent : Natural) is
   begin
      null;
   end Disp_PSL_NFA;

   function Image_Location_Type (Loc : Location_Type) return String is
   begin
      return Errorout.Get_Location_Str (Loc);
   end Image_Location_Type;

   function Image_Iir_Direction (Dir : Iir_Direction) return String is
   begin
      case Dir is
         when Iir_To =>
            return "to";
         when Iir_Downto =>
            return "downto";
      end case;
   end Image_Iir_Direction;

   function Image_Token_Type (Tok : Tokens.Token_Type) return String
     renames Tokens.Image;

   procedure Header (Str : String; Indent : Natural) is
   begin
      Put_Indent (Indent);
      Put (Str);
   end Header;

   --  Subprograms
   procedure Disp_Header (N : Iir) is
   begin
      if N = Null_Iir then
         Put_Line ("*null*");
         return;
      end if;

      case Get_Kind (N) is
         when Iir_Kind_Unused =>
            Put ("unused");
         when Iir_Kind_Error =>
            Put ("error");
         when Iir_Kind_Design_File =>
            Put ("design_file");
         when Iir_Kind_Design_Unit =>
            Put ("design_unit " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Library_Clause =>
            Put ("library_clause " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Use_Clause =>
            Put ("use_clause");
         when Iir_Kind_Integer_Literal =>
            Put ("integer_literal");
         when Iir_Kind_Floating_Point_Literal =>
            Put ("floating_point_literal");
         when Iir_Kind_Null_Literal =>
            Put ("null_literal");
         when Iir_Kind_String_Literal =>
            Put ("string_literal");
         when Iir_Kind_Physical_Int_Literal =>
            Put ("physical_int_literal");
         when Iir_Kind_Physical_Fp_Literal =>
            Put ("physical_fp_literal");
         when Iir_Kind_Bit_String_Literal =>
            Put ("bit_string_literal");
         when Iir_Kind_Simple_Aggregate =>
            Put ("simple_aggregate");
         when Iir_Kind_Overflow_Literal =>
            Put ("overflow_literal");
         when Iir_Kind_Waveform_Element =>
            Put ("waveform_element");
         when Iir_Kind_Conditional_Waveform =>
            Put ("conditional_waveform");
         when Iir_Kind_Association_Element_By_Expression =>
            Put ("association_element_by_expression");
         when Iir_Kind_Association_Element_By_Individual =>
            Put ("association_element_by_individual");
         when Iir_Kind_Association_Element_Open =>
            Put ("association_element_open");
         when Iir_Kind_Choice_By_Others =>
            Put ("choice_by_others");
         when Iir_Kind_Choice_By_Expression =>
            Put ("choice_by_expression");
         when Iir_Kind_Choice_By_Range =>
            Put ("choice_by_range");
         when Iir_Kind_Choice_By_None =>
            Put ("choice_by_none");
         when Iir_Kind_Choice_By_Name =>
            Put ("choice_by_name");
         when Iir_Kind_Entity_Aspect_Entity =>
            Put ("entity_aspect_entity");
         when Iir_Kind_Entity_Aspect_Configuration =>
            Put ("entity_aspect_configuration");
         when Iir_Kind_Entity_Aspect_Open =>
            Put ("entity_aspect_open");
         when Iir_Kind_Block_Configuration =>
            Put ("block_configuration");
         when Iir_Kind_Block_Header =>
            Put ("block_header");
         when Iir_Kind_Component_Configuration =>
            Put ("component_configuration");
         when Iir_Kind_Binding_Indication =>
            Put ("binding_indication");
         when Iir_Kind_Entity_Class =>
            Put ("entity_class");
         when Iir_Kind_Attribute_Value =>
            Put ("attribute_value");
         when Iir_Kind_Signature =>
            Put ("signature");
         when Iir_Kind_Aggregate_Info =>
            Put ("aggregate_info");
         when Iir_Kind_Procedure_Call =>
            Put ("procedure_call");
         when Iir_Kind_Record_Element_Constraint =>
            Put ("record_element_constraint " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Attribute_Specification =>
            Put ("attribute_specification");
         when Iir_Kind_Disconnection_Specification =>
            Put ("disconnection_specification");
         when Iir_Kind_Configuration_Specification =>
            Put ("configuration_specification");
         when Iir_Kind_Access_Type_Definition =>
            Put ("access_type_definition");
         when Iir_Kind_Incomplete_Type_Definition =>
            Put ("incomplete_type_definition");
         when Iir_Kind_File_Type_Definition =>
            Put ("file_type_definition");
         when Iir_Kind_Protected_Type_Declaration =>
            Put ("protected_type_declaration");
         when Iir_Kind_Record_Type_Definition =>
            Put ("record_type_definition");
         when Iir_Kind_Array_Type_Definition =>
            Put ("array_type_definition");
         when Iir_Kind_Array_Subtype_Definition =>
            Put ("array_subtype_definition");
         when Iir_Kind_Record_Subtype_Definition =>
            Put ("record_subtype_definition");
         when Iir_Kind_Access_Subtype_Definition =>
            Put ("access_subtype_definition");
         when Iir_Kind_Physical_Subtype_Definition =>
            Put ("physical_subtype_definition");
         when Iir_Kind_Floating_Subtype_Definition =>
            Put ("floating_subtype_definition");
         when Iir_Kind_Integer_Subtype_Definition =>
            Put ("integer_subtype_definition");
         when Iir_Kind_Enumeration_Subtype_Definition =>
            Put ("enumeration_subtype_definition");
         when Iir_Kind_Enumeration_Type_Definition =>
            Put ("enumeration_type_definition");
         when Iir_Kind_Integer_Type_Definition =>
            Put ("integer_type_definition");
         when Iir_Kind_Floating_Type_Definition =>
            Put ("floating_type_definition");
         when Iir_Kind_Physical_Type_Definition =>
            Put ("physical_type_definition");
         when Iir_Kind_Range_Expression =>
            Put ("range_expression");
         when Iir_Kind_Protected_Type_Body =>
            Put ("protected_type_body " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Subtype_Definition =>
            Put ("subtype_definition");
         when Iir_Kind_Scalar_Nature_Definition =>
            Put ("scalar_nature_definition");
         when Iir_Kind_Overload_List =>
            Put ("overload_list");
         when Iir_Kind_Type_Declaration =>
            Put ("type_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Anonymous_Type_Declaration =>
            Put ("anonymous_type_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Subtype_Declaration =>
            Put ("subtype_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Nature_Declaration =>
            Put ("nature_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Subnature_Declaration =>
            Put ("subnature_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Configuration_Declaration =>
            Put ("configuration_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Entity_Declaration =>
            Put ("entity_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Package_Declaration =>
            Put ("package_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Package_Body =>
            Put ("package_body " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Architecture_Body =>
            Put ("architecture_body " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Package_Instantiation_Declaration =>
            Put ("package_instantiation_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Package_Header =>
            Put ("package_header");
         when Iir_Kind_Unit_Declaration =>
            Put ("unit_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Library_Declaration =>
            Put ("library_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Component_Declaration =>
            Put ("component_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Attribute_Declaration =>
            Put ("attribute_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Group_Template_Declaration =>
            Put ("group_template_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Group_Declaration =>
            Put ("group_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Element_Declaration =>
            Put ("element_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Non_Object_Alias_Declaration =>
            Put ("non_object_alias_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Psl_Declaration =>
            Put ("psl_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Terminal_Declaration =>
            Put ("terminal_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Free_Quantity_Declaration =>
            Put ("free_quantity_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Across_Quantity_Declaration =>
            Put ("across_quantity_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Through_Quantity_Declaration =>
            Put ("through_quantity_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Enumeration_Literal =>
            Put ("enumeration_literal " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Function_Declaration =>
            Put ("function_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Implicit_Function_Declaration =>
            Put ("implicit_function_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Implicit_Procedure_Declaration =>
            Put ("implicit_procedure_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Procedure_Declaration =>
            Put ("procedure_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Function_Body =>
            Put ("function_body");
         when Iir_Kind_Procedure_Body =>
            Put ("procedure_body");
         when Iir_Kind_Object_Alias_Declaration =>
            Put ("object_alias_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_File_Declaration =>
            Put ("file_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Guard_Signal_Declaration =>
            Put ("guard_signal_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Signal_Declaration =>
            Put ("signal_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Variable_Declaration =>
            Put ("variable_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Constant_Declaration =>
            Put ("constant_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Iterator_Declaration =>
            Put ("iterator_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Constant_Interface_Declaration =>
            Put ("constant_interface_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Variable_Interface_Declaration =>
            Put ("variable_interface_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Signal_Interface_Declaration =>
            Put ("signal_interface_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_File_Interface_Declaration =>
            Put ("file_interface_declaration " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Identity_Operator =>
            Put ("identity_operator");
         when Iir_Kind_Negation_Operator =>
            Put ("negation_operator");
         when Iir_Kind_Absolute_Operator =>
            Put ("absolute_operator");
         when Iir_Kind_Not_Operator =>
            Put ("not_operator");
         when Iir_Kind_Condition_Operator =>
            Put ("condition_operator");
         when Iir_Kind_Reduction_And_Operator =>
            Put ("reduction_and_operator");
         when Iir_Kind_Reduction_Or_Operator =>
            Put ("reduction_or_operator");
         when Iir_Kind_Reduction_Nand_Operator =>
            Put ("reduction_nand_operator");
         when Iir_Kind_Reduction_Nor_Operator =>
            Put ("reduction_nor_operator");
         when Iir_Kind_Reduction_Xor_Operator =>
            Put ("reduction_xor_operator");
         when Iir_Kind_Reduction_Xnor_Operator =>
            Put ("reduction_xnor_operator");
         when Iir_Kind_And_Operator =>
            Put ("and_operator");
         when Iir_Kind_Or_Operator =>
            Put ("or_operator");
         when Iir_Kind_Nand_Operator =>
            Put ("nand_operator");
         when Iir_Kind_Nor_Operator =>
            Put ("nor_operator");
         when Iir_Kind_Xor_Operator =>
            Put ("xor_operator");
         when Iir_Kind_Xnor_Operator =>
            Put ("xnor_operator");
         when Iir_Kind_Equality_Operator =>
            Put ("equality_operator");
         when Iir_Kind_Inequality_Operator =>
            Put ("inequality_operator");
         when Iir_Kind_Less_Than_Operator =>
            Put ("less_than_operator");
         when Iir_Kind_Less_Than_Or_Equal_Operator =>
            Put ("less_than_or_equal_operator");
         when Iir_Kind_Greater_Than_Operator =>
            Put ("greater_than_operator");
         when Iir_Kind_Greater_Than_Or_Equal_Operator =>
            Put ("greater_than_or_equal_operator");
         when Iir_Kind_Match_Equality_Operator =>
            Put ("match_equality_operator");
         when Iir_Kind_Match_Inequality_Operator =>
            Put ("match_inequality_operator");
         when Iir_Kind_Match_Less_Than_Operator =>
            Put ("match_less_than_operator");
         when Iir_Kind_Match_Less_Than_Or_Equal_Operator =>
            Put ("match_less_than_or_equal_operator");
         when Iir_Kind_Match_Greater_Than_Operator =>
            Put ("match_greater_than_operator");
         when Iir_Kind_Match_Greater_Than_Or_Equal_Operator =>
            Put ("match_greater_than_or_equal_operator");
         when Iir_Kind_Sll_Operator =>
            Put ("sll_operator");
         when Iir_Kind_Sla_Operator =>
            Put ("sla_operator");
         when Iir_Kind_Srl_Operator =>
            Put ("srl_operator");
         when Iir_Kind_Sra_Operator =>
            Put ("sra_operator");
         when Iir_Kind_Rol_Operator =>
            Put ("rol_operator");
         when Iir_Kind_Ror_Operator =>
            Put ("ror_operator");
         when Iir_Kind_Addition_Operator =>
            Put ("addition_operator");
         when Iir_Kind_Substraction_Operator =>
            Put ("substraction_operator");
         when Iir_Kind_Concatenation_Operator =>
            Put ("concatenation_operator");
         when Iir_Kind_Multiplication_Operator =>
            Put ("multiplication_operator");
         when Iir_Kind_Division_Operator =>
            Put ("division_operator");
         when Iir_Kind_Modulus_Operator =>
            Put ("modulus_operator");
         when Iir_Kind_Remainder_Operator =>
            Put ("remainder_operator");
         when Iir_Kind_Exponentiation_Operator =>
            Put ("exponentiation_operator");
         when Iir_Kind_Function_Call =>
            Put ("function_call");
         when Iir_Kind_Aggregate =>
            Put ("aggregate");
         when Iir_Kind_Parenthesis_Expression =>
            Put ("parenthesis_expression");
         when Iir_Kind_Qualified_Expression =>
            Put ("qualified_expression");
         when Iir_Kind_Type_Conversion =>
            Put ("type_conversion");
         when Iir_Kind_Allocator_By_Expression =>
            Put ("allocator_by_expression");
         when Iir_Kind_Allocator_By_Subtype =>
            Put ("allocator_by_subtype");
         when Iir_Kind_Selected_Element =>
            Put ("selected_element");
         when Iir_Kind_Dereference =>
            Put ("dereference");
         when Iir_Kind_Implicit_Dereference =>
            Put ("implicit_dereference");
         when Iir_Kind_Slice_Name =>
            Put ("slice_name");
         when Iir_Kind_Indexed_Name =>
            Put ("indexed_name");
         when Iir_Kind_Psl_Expression =>
            Put ("psl_expression");
         when Iir_Kind_Sensitized_Process_Statement =>
            Put ("sensitized_process_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Process_Statement =>
            Put ("process_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
            Put ("concurrent_conditional_signal_assignment " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Concurrent_Selected_Signal_Assignment =>
            Put ("concurrent_selected_signal_assignment " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Concurrent_Assertion_Statement =>
            Put ("concurrent_assertion_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Psl_Default_Clock =>
            Put ("psl_default_clock " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Psl_Assert_Statement =>
            Put ("psl_assert_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Psl_Cover_Statement =>
            Put ("psl_cover_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Concurrent_Procedure_Call_Statement =>
            Put ("concurrent_procedure_call_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Block_Statement =>
            Put ("block_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Generate_Statement =>
            Put ("generate_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Component_Instantiation_Statement =>
            Put ("component_instantiation_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Simple_Simultaneous_Statement =>
            Put ("simple_simultaneous_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Signal_Assignment_Statement =>
            Put ("signal_assignment_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Null_Statement =>
            Put ("null_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Assertion_Statement =>
            Put ("assertion_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Report_Statement =>
            Put ("report_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Wait_Statement =>
            Put ("wait_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Variable_Assignment_Statement =>
            Put ("variable_assignment_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Return_Statement =>
            Put ("return_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_For_Loop_Statement =>
            Put ("for_loop_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_While_Loop_Statement =>
            Put ("while_loop_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Next_Statement =>
            Put ("next_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Exit_Statement =>
            Put ("exit_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Case_Statement =>
            Put ("case_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Procedure_Call_Statement =>
            Put ("procedure_call_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_If_Statement =>
            Put ("if_statement " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Elsif =>
            Put ("elsif");
         when Iir_Kind_Character_Literal =>
            Put ("character_literal " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Simple_Name =>
            Put ("simple_name " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Selected_Name =>
            Put ("selected_name " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Operator_Symbol =>
            Put ("operator_symbol " &
                      Image_Name_Id (Get_Identifier (N)));
         when Iir_Kind_Selected_By_All_Name =>
            Put ("selected_by_all_name");
         when Iir_Kind_Parenthesis_Name =>
            Put ("parenthesis_name");
         when Iir_Kind_Base_Attribute =>
            Put ("base_attribute");
         when Iir_Kind_Left_Type_Attribute =>
            Put ("left_type_attribute");
         when Iir_Kind_Right_Type_Attribute =>
            Put ("right_type_attribute");
         when Iir_Kind_High_Type_Attribute =>
            Put ("high_type_attribute");
         when Iir_Kind_Low_Type_Attribute =>
            Put ("low_type_attribute");
         when Iir_Kind_Ascending_Type_Attribute =>
            Put ("ascending_type_attribute");
         when Iir_Kind_Image_Attribute =>
            Put ("image_attribute");
         when Iir_Kind_Value_Attribute =>
            Put ("value_attribute");
         when Iir_Kind_Pos_Attribute =>
            Put ("pos_attribute");
         when Iir_Kind_Val_Attribute =>
            Put ("val_attribute");
         when Iir_Kind_Succ_Attribute =>
            Put ("succ_attribute");
         when Iir_Kind_Pred_Attribute =>
            Put ("pred_attribute");
         when Iir_Kind_Leftof_Attribute =>
            Put ("leftof_attribute");
         when Iir_Kind_Rightof_Attribute =>
            Put ("rightof_attribute");
         when Iir_Kind_Delayed_Attribute =>
            Put ("delayed_attribute");
         when Iir_Kind_Stable_Attribute =>
            Put ("stable_attribute");
         when Iir_Kind_Quiet_Attribute =>
            Put ("quiet_attribute");
         when Iir_Kind_Transaction_Attribute =>
            Put ("transaction_attribute");
         when Iir_Kind_Event_Attribute =>
            Put ("event_attribute");
         when Iir_Kind_Active_Attribute =>
            Put ("active_attribute");
         when Iir_Kind_Last_Event_Attribute =>
            Put ("last_event_attribute");
         when Iir_Kind_Last_Active_Attribute =>
            Put ("last_active_attribute");
         when Iir_Kind_Last_Value_Attribute =>
            Put ("last_value_attribute");
         when Iir_Kind_Driving_Attribute =>
            Put ("driving_attribute");
         when Iir_Kind_Driving_Value_Attribute =>
            Put ("driving_value_attribute");
         when Iir_Kind_Behavior_Attribute =>
            Put ("behavior_attribute");
         when Iir_Kind_Structure_Attribute =>
            Put ("structure_attribute");
         when Iir_Kind_Simple_Name_Attribute =>
            Put ("simple_name_attribute");
         when Iir_Kind_Instance_Name_Attribute =>
            Put ("instance_name_attribute");
         when Iir_Kind_Path_Name_Attribute =>
            Put ("path_name_attribute");
         when Iir_Kind_Left_Array_Attribute =>
            Put ("left_array_attribute");
         when Iir_Kind_Right_Array_Attribute =>
            Put ("right_array_attribute");
         when Iir_Kind_High_Array_Attribute =>
            Put ("high_array_attribute");
         when Iir_Kind_Low_Array_Attribute =>
            Put ("low_array_attribute");
         when Iir_Kind_Length_Array_Attribute =>
            Put ("length_array_attribute");
         when Iir_Kind_Ascending_Array_Attribute =>
            Put ("ascending_array_attribute");
         when Iir_Kind_Range_Array_Attribute =>
            Put ("range_array_attribute");
         when Iir_Kind_Reverse_Range_Array_Attribute =>
            Put ("reverse_range_array_attribute");
         when Iir_Kind_Attribute_Name =>
            Put ("attribute_name " &
                      Image_Name_Id (Get_Identifier (N)));
      end case;
      Put (' ');
      Disp_Iir_Number (N);
      New_Line;
   end Disp_Header;

   procedure Disp_Iir (N : Iir;
                       Indent : Natural := 1;
                       Flat : Boolean := False)
   is
      Sub_Indent : constant Natural := Indent + 1;
   begin
      Disp_Header (N);

      if Flat or else N = Null_Iir then
         return;
      end if;

      Header ("location: ", Indent);
      Put_Line (Image_Location_Type (Get_Location (N)));

      --  Protect against infinite recursions.
      if Indent > 20 then
         Put_Indent (Indent);
         Put_Line ("...");
         return;
      end if;

      case Get_Kind (N) is
         when Iir_Kind_Unused
           | Iir_Kind_Entity_Aspect_Open
           | Iir_Kind_Behavior_Attribute
           | Iir_Kind_Structure_Attribute =>
            null;
         when Iir_Kind_Error =>
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("error_origin: ", Indent);
            Disp_Iir (Get_Error_Origin (N), Sub_Indent);
            Header ("type_declarator: ", Indent);
            Disp_Iir (Get_Type_Declarator (N), Sub_Indent, True);
            Header ("base_type: ", Indent);
            Disp_Iir (Get_Base_Type (N), Sub_Indent, True);
            Header ("resolved_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Resolved_Flag (N)));
            Header ("signal_type_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Signal_Type_Flag (N)));
            Header ("has_signal_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Signal_Flag (N)));
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
         when Iir_Kind_Design_File =>
            Header ("library: ", Indent);
            Disp_Iir (Get_Library (N), Sub_Indent, True);
            Header ("file_dependence_list: ", Indent);
            Disp_Iir_List (Get_File_Dependence_List (N), Sub_Indent);
            Header ("design_file_directory: ", Indent);
            Put_Line (Image_Name_Id (Get_Design_File_Directory (N)));
            Header ("design_file_filename: ", Indent);
            Put_Line (Image_Name_Id (Get_Design_File_Filename (N)));
            Header ("analysis_time_stamp: ", Indent);
            Put_Line (Image_Time_Stamp_Id (Get_Analysis_Time_Stamp (N)));
            Header ("file_time_stamp: ", Indent);
            Put_Line (Image_Time_Stamp_Id (Get_File_Time_Stamp (N)));
            Header ("first_design_unit: ", Indent);
            Disp_Chain (Get_First_Design_Unit (N), Sub_Indent);
            Header ("last_design_unit: ", Indent);
            Disp_Iir (Get_Last_Design_Unit (N), Sub_Indent, True);
            Header ("elab_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Elab_Flag (N)));
         when Iir_Kind_Design_Unit =>
            Header ("design_file: ", Indent);
            Disp_Iir (Get_Design_File (N), Sub_Indent, True);
            Header ("context_items: ", Indent);
            Disp_Chain (Get_Context_Items (N), Sub_Indent);
            Header ("date: ", Indent);
            Put_Line (Date_Type'Image (Get_Date (N)));
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("library_unit: ", Indent);
            Disp_Iir (Get_Library_Unit (N), Sub_Indent);
            Header ("end_location: ", Indent);
            Put_Line (Image_Location_Type (Get_End_Location (N)));
            Header ("hash_chain: ", Indent);
            Disp_Iir (Get_Hash_Chain (N), Sub_Indent, True);
            Header ("dependence_list: ", Indent);
            Disp_Iir_List (Get_Dependence_List (N), Sub_Indent, True);
            Header ("analysis_checks_list: ", Indent);
            Disp_Iir_List (Get_Analysis_Checks_List (N), Sub_Indent);
            Header ("elab_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Elab_Flag (N)));
            Header ("date_state: ", Indent);
            Put_Line (Image_Date_State_Type (Get_Date_State (N)));
         when Iir_Kind_Library_Clause =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("library_declaration: ", Indent);
            Disp_Iir (Get_Library_Declaration (N), Sub_Indent);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("has_identifier_list: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Identifier_List (N)));
         when Iir_Kind_Use_Clause =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("selected_name: ", Indent);
            Disp_Iir (Get_Selected_Name (N), Sub_Indent);
            Header ("use_clause_chain: ", Indent);
            Disp_Iir (Get_Use_Clause_Chain (N), Sub_Indent);
         when Iir_Kind_Integer_Literal =>
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("literal_origin: ", Indent);
            Disp_Iir (Get_Literal_Origin (N), Sub_Indent);
            Header ("value: ", Indent);
            Put_Line (Iir_Int64'Image (Get_Value (N)));
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
         when Iir_Kind_Floating_Point_Literal =>
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("literal_origin: ", Indent);
            Disp_Iir (Get_Literal_Origin (N), Sub_Indent);
            Header ("fp_value: ", Indent);
            Put_Line (Iir_Fp64'Image (Get_Fp_Value (N)));
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
         when Iir_Kind_Null_Literal =>
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
         when Iir_Kind_String_Literal =>
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("literal_origin: ", Indent);
            Disp_Iir (Get_Literal_Origin (N), Sub_Indent);
            Header ("string_id: ", Indent);
            Put_Line (Image_String_Id (Get_String_Id (N)));
            Header ("string_length: ", Indent);
            Put_Line (Int32'Image (Get_String_Length (N)));
            Header ("literal_subtype: ", Indent);
            Disp_Iir (Get_Literal_Subtype (N), Sub_Indent);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
         when Iir_Kind_Physical_Int_Literal =>
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("literal_origin: ", Indent);
            Disp_Iir (Get_Literal_Origin (N), Sub_Indent);
            Header ("unit_name: ", Indent);
            Disp_Iir (Get_Unit_Name (N), Sub_Indent);
            Header ("value: ", Indent);
            Put_Line (Iir_Int64'Image (Get_Value (N)));
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
         when Iir_Kind_Physical_Fp_Literal =>
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("literal_origin: ", Indent);
            Disp_Iir (Get_Literal_Origin (N), Sub_Indent);
            Header ("unit_name: ", Indent);
            Disp_Iir (Get_Unit_Name (N), Sub_Indent);
            Header ("fp_value: ", Indent);
            Put_Line (Iir_Fp64'Image (Get_Fp_Value (N)));
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
         when Iir_Kind_Bit_String_Literal =>
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("literal_origin: ", Indent);
            Disp_Iir (Get_Literal_Origin (N), Sub_Indent);
            Header ("string_id: ", Indent);
            Put_Line (Image_String_Id (Get_String_Id (N)));
            Header ("string_length: ", Indent);
            Put_Line (Int32'Image (Get_String_Length (N)));
            Header ("literal_subtype: ", Indent);
            Disp_Iir (Get_Literal_Subtype (N), Sub_Indent);
            Header ("bit_string_0: ", Indent);
            Disp_Iir (Get_Bit_String_0 (N), Sub_Indent);
            Header ("bit_string_1: ", Indent);
            Disp_Iir (Get_Bit_String_1 (N), Sub_Indent);
            Header ("bit_string_base: ", Indent);
            Put_Line (Base_Type'Image (Get_Bit_String_Base (N)));
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
         when Iir_Kind_Simple_Aggregate =>
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("literal_origin: ", Indent);
            Disp_Iir (Get_Literal_Origin (N), Sub_Indent);
            Header ("simple_aggregate_list: ", Indent);
            Disp_Iir_List (Get_Simple_Aggregate_List (N), Sub_Indent);
            Header ("literal_subtype: ", Indent);
            Disp_Iir (Get_Literal_Subtype (N), Sub_Indent);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
         when Iir_Kind_Overflow_Literal =>
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("literal_origin: ", Indent);
            Disp_Iir (Get_Literal_Origin (N), Sub_Indent);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
         when Iir_Kind_Waveform_Element =>
            Header ("we_value: ", Indent);
            Disp_Iir (Get_We_Value (N), Sub_Indent);
            Header ("time: ", Indent);
            Disp_Iir (Get_Time (N), Sub_Indent);
         when Iir_Kind_Conditional_Waveform =>
            Header ("condition: ", Indent);
            Disp_Iir (Get_Condition (N), Sub_Indent);
            Header ("waveform_chain: ", Indent);
            Disp_Chain (Get_Waveform_Chain (N), Sub_Indent);
         when Iir_Kind_Association_Element_By_Expression =>
            Header ("formal: ", Indent);
            Disp_Iir (Get_Formal (N), Sub_Indent);
            Header ("actual: ", Indent);
            Disp_Iir (Get_Actual (N), Sub_Indent);
            Header ("in_conversion: ", Indent);
            Disp_Iir (Get_In_Conversion (N), Sub_Indent);
            Header ("out_conversion: ", Indent);
            Disp_Iir (Get_Out_Conversion (N), Sub_Indent);
            Header ("whole_association_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Whole_Association_Flag (N)));
            Header ("collapse_signal_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Collapse_Signal_Flag (N)));
         when Iir_Kind_Association_Element_By_Individual =>
            Header ("formal: ", Indent);
            Disp_Iir (Get_Formal (N), Sub_Indent);
            Header ("actual_type: ", Indent);
            Disp_Iir (Get_Actual_Type (N), Sub_Indent);
            Header ("individual_association_chain: ", Indent);
            Disp_Chain (Get_Individual_Association_Chain (N), Sub_Indent);
            Header ("whole_association_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Whole_Association_Flag (N)));
            Header ("collapse_signal_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Collapse_Signal_Flag (N)));
         when Iir_Kind_Association_Element_Open =>
            Header ("formal: ", Indent);
            Disp_Iir (Get_Formal (N), Sub_Indent);
            Header ("whole_association_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Whole_Association_Flag (N)));
            Header ("collapse_signal_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Collapse_Signal_Flag (N)));
            Header ("artificial_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Artificial_Flag (N)));
         when Iir_Kind_Choice_By_Others
           | Iir_Kind_Choice_By_None =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("associated_expr: ", Indent);
            Disp_Iir (Get_Associated_Expr (N), Sub_Indent);
            Header ("associated_chain: ", Indent);
            Disp_Chain (Get_Associated_Chain (N), Sub_Indent);
            Header ("same_alternative_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Same_Alternative_Flag (N)));
         when Iir_Kind_Choice_By_Expression =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("associated_expr: ", Indent);
            Disp_Iir (Get_Associated_Expr (N), Sub_Indent);
            Header ("associated_chain: ", Indent);
            Disp_Chain (Get_Associated_Chain (N), Sub_Indent);
            Header ("choice_expression: ", Indent);
            Disp_Iir (Get_Choice_Expression (N), Sub_Indent);
            Header ("same_alternative_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Same_Alternative_Flag (N)));
            Header ("choice_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Choice_Staticness (N)));
         when Iir_Kind_Choice_By_Range =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("associated_expr: ", Indent);
            Disp_Iir (Get_Associated_Expr (N), Sub_Indent);
            Header ("associated_chain: ", Indent);
            Disp_Chain (Get_Associated_Chain (N), Sub_Indent);
            Header ("choice_range: ", Indent);
            Disp_Iir (Get_Choice_Range (N), Sub_Indent);
            Header ("same_alternative_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Same_Alternative_Flag (N)));
            Header ("choice_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Choice_Staticness (N)));
         when Iir_Kind_Choice_By_Name =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("associated_expr: ", Indent);
            Disp_Iir (Get_Associated_Expr (N), Sub_Indent);
            Header ("associated_chain: ", Indent);
            Disp_Chain (Get_Associated_Chain (N), Sub_Indent);
            Header ("choice_name: ", Indent);
            Disp_Iir (Get_Choice_Name (N), Sub_Indent);
            Header ("same_alternative_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Same_Alternative_Flag (N)));
         when Iir_Kind_Entity_Aspect_Entity =>
            Header ("entity_name: ", Indent);
            Disp_Iir (Get_Entity_Name (N), Sub_Indent);
            Header ("architecture: ", Indent);
            Disp_Iir (Get_Architecture (N), Sub_Indent);
         when Iir_Kind_Entity_Aspect_Configuration =>
            Header ("configuration_name: ", Indent);
            Disp_Iir (Get_Configuration_Name (N), Sub_Indent);
         when Iir_Kind_Block_Configuration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("declaration_chain: ", Indent);
            Disp_Chain (Get_Declaration_Chain (N), Sub_Indent);
            Header ("configuration_item_chain: ", Indent);
            Disp_Iir (Get_Configuration_Item_Chain (N), Sub_Indent);
            Header ("prev_block_configuration: ", Indent);
            Disp_Iir (Get_Prev_Block_Configuration (N), Sub_Indent, True);
            Header ("block_specification: ", Indent);
            Disp_Iir (Get_Block_Specification (N), Sub_Indent);
         when Iir_Kind_Block_Header =>
            Header ("generic_chain: ", Indent);
            Disp_Chain (Get_Generic_Chain (N), Sub_Indent);
            Header ("port_chain: ", Indent);
            Disp_Chain (Get_Port_Chain (N), Sub_Indent);
            Header ("generic_map_aspect_chain: ", Indent);
            Disp_Chain (Get_Generic_Map_Aspect_Chain (N), Sub_Indent);
            Header ("port_map_aspect_chain: ", Indent);
            Disp_Chain (Get_Port_Map_Aspect_Chain (N), Sub_Indent);
         when Iir_Kind_Component_Configuration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("instantiation_list: ", Indent);
            Disp_Iir_List (Get_Instantiation_List (N), Sub_Indent);
            Header ("binding_indication: ", Indent);
            Disp_Iir (Get_Binding_Indication (N), Sub_Indent);
            Header ("component_name: ", Indent);
            Disp_Iir (Get_Component_Name (N), Sub_Indent);
            Header ("block_configuration: ", Indent);
            Disp_Iir (Get_Block_Configuration (N), Sub_Indent);
         when Iir_Kind_Binding_Indication =>
            Header ("default_entity_aspect: ", Indent);
            Disp_Iir (Get_Default_Entity_Aspect (N), Sub_Indent);
            Header ("entity_aspect: ", Indent);
            Disp_Iir (Get_Entity_Aspect (N), Sub_Indent);
            Header ("default_generic_map_aspect_chain: ", Indent);
            Disp_Chain (Get_Default_Generic_Map_Aspect_Chain (N), Sub_Indent);
            Header ("default_port_map_aspect_chain: ", Indent);
            Disp_Chain (Get_Default_Port_Map_Aspect_Chain (N), Sub_Indent);
            Header ("generic_map_aspect_chain: ", Indent);
            Disp_Chain (Get_Generic_Map_Aspect_Chain (N), Sub_Indent);
            Header ("port_map_aspect_chain: ", Indent);
            Disp_Chain (Get_Port_Map_Aspect_Chain (N), Sub_Indent);
         when Iir_Kind_Entity_Class =>
            Header ("entity_class: ", Indent);
            Put_Line (Image_Token_Type (Get_Entity_Class (N)));
         when Iir_Kind_Attribute_Value =>
            Header ("spec_chain: ", Indent);
            Disp_Iir (Get_Spec_Chain (N), Sub_Indent);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("designated_entity: ", Indent);
            Disp_Iir (Get_Designated_Entity (N), Sub_Indent, True);
            Header ("attribute_specification: ", Indent);
            Disp_Iir (Get_Attribute_Specification (N), Sub_Indent, True);
            Header ("base_name: ", Indent);
            Disp_Iir (Get_Base_Name (N), Sub_Indent, True);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
         when Iir_Kind_Signature =>
            Header ("prefix: ", Indent);
            Disp_Iir (Get_Prefix (N), Sub_Indent);
            Header ("type_marks_list: ", Indent);
            Disp_Iir_List (Get_Type_Marks_List (N), Sub_Indent);
            Header ("return_type_mark: ", Indent);
            Disp_Iir (Get_Return_Type_Mark (N), Sub_Indent);
         when Iir_Kind_Aggregate_Info =>
            Header ("sub_aggregate_info: ", Indent);
            Disp_Iir (Get_Sub_Aggregate_Info (N), Sub_Indent);
            Header ("aggr_low_limit: ", Indent);
            Disp_Iir (Get_Aggr_Low_Limit (N), Sub_Indent);
            Header ("aggr_high_limit: ", Indent);
            Disp_Iir (Get_Aggr_High_Limit (N), Sub_Indent);
            Header ("aggr_min_length: ", Indent);
            Put_Line (Iir_Int32'Image (Get_Aggr_Min_Length (N)));
            Header ("aggr_others_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Aggr_Others_Flag (N)));
            Header ("aggr_dynamic_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Aggr_Dynamic_Flag (N)));
            Header ("aggr_named_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Aggr_Named_Flag (N)));
         when Iir_Kind_Procedure_Call =>
            Header ("prefix: ", Indent);
            Disp_Iir (Get_Prefix (N), Sub_Indent);
            Header ("parameter_association_chain: ", Indent);
            Disp_Chain (Get_Parameter_Association_Chain (N), Sub_Indent);
            Header ("implementation: ", Indent);
            Disp_Iir (Get_Implementation (N), Sub_Indent, True);
            Header ("method_object: ", Indent);
            Disp_Iir (Get_Method_Object (N), Sub_Indent);
         when Iir_Kind_Record_Element_Constraint =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("element_declaration: ", Indent);
            Disp_Iir (Get_Element_Declaration (N), Sub_Indent);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("element_position: ", Indent);
            Put_Line (Iir_Index32'Image (Get_Element_Position (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
         when Iir_Kind_Attribute_Specification =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("entity_name_list: ", Indent);
            Disp_Iir_List (Get_Entity_Name_List (N), Sub_Indent);
            Header ("entity_class: ", Indent);
            Put_Line (Image_Token_Type (Get_Entity_Class (N)));
            Header ("attribute_value_spec_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Spec_Chain (N), Sub_Indent);
            Header ("expression: ", Indent);
            Disp_Iir (Get_Expression (N), Sub_Indent);
            Header ("attribute_designator: ", Indent);
            Disp_Iir (Get_Attribute_Designator (N), Sub_Indent);
            Header ("attribute_specification_chain: ", Indent);
            Disp_Iir (Get_Attribute_Specification_Chain (N), Sub_Indent);
         when Iir_Kind_Disconnection_Specification =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("signal_list: ", Indent);
            Disp_Iir_List (Get_Signal_List (N), Sub_Indent);
            Header ("type_mark: ", Indent);
            Disp_Iir (Get_Type_Mark (N), Sub_Indent);
            Header ("expression: ", Indent);
            Disp_Iir (Get_Expression (N), Sub_Indent);
         when Iir_Kind_Configuration_Specification =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("instantiation_list: ", Indent);
            Disp_Iir_List (Get_Instantiation_List (N), Sub_Indent);
            Header ("binding_indication: ", Indent);
            Disp_Iir (Get_Binding_Indication (N), Sub_Indent);
            Header ("component_name: ", Indent);
            Disp_Iir (Get_Component_Name (N), Sub_Indent);
         when Iir_Kind_Access_Type_Definition =>
            Header ("designated_type: ", Indent);
            Disp_Iir (Get_Designated_Type (N), Sub_Indent, True);
            Header ("type_declarator: ", Indent);
            Disp_Iir (Get_Type_Declarator (N), Sub_Indent, True);
            Header ("base_type: ", Indent);
            Disp_Iir (Get_Base_Type (N), Sub_Indent, True);
            Header ("designated_subtype_indication: ", Indent);
            Disp_Iir (Get_Designated_Subtype_Indication (N), Sub_Indent);
            Header ("resolved_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Resolved_Flag (N)));
            Header ("signal_type_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Signal_Type_Flag (N)));
            Header ("type_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Type_Staticness (N)));
         when Iir_Kind_Incomplete_Type_Definition =>
            Header ("incomplete_type_list: ", Indent);
            Disp_Iir_List (Get_Incomplete_Type_List (N), Sub_Indent);
            Header ("type_declarator: ", Indent);
            Disp_Iir (Get_Type_Declarator (N), Sub_Indent, True);
            Header ("base_type: ", Indent);
            Disp_Iir (Get_Base_Type (N), Sub_Indent, True);
            Header ("resolved_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Resolved_Flag (N)));
            Header ("signal_type_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Signal_Type_Flag (N)));
            Header ("has_signal_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Signal_Flag (N)));
            Header ("type_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Type_Staticness (N)));
         when Iir_Kind_File_Type_Definition =>
            Header ("file_type_mark: ", Indent);
            Disp_Iir (Get_File_Type_Mark (N), Sub_Indent);
            Header ("type_declarator: ", Indent);
            Disp_Iir (Get_Type_Declarator (N), Sub_Indent, True);
            Header ("base_type: ", Indent);
            Disp_Iir (Get_Base_Type (N), Sub_Indent, True);
            Header ("resolved_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Resolved_Flag (N)));
            Header ("signal_type_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Signal_Type_Flag (N)));
            Header ("text_file_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Text_File_Flag (N)));
            Header ("type_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Type_Staticness (N)));
         when Iir_Kind_Protected_Type_Declaration =>
            Header ("declaration_chain: ", Indent);
            Disp_Chain (Get_Declaration_Chain (N), Sub_Indent);
            Header ("protected_type_body: ", Indent);
            Disp_Iir (Get_Protected_Type_Body (N), Sub_Indent);
            Header ("type_declarator: ", Indent);
            Disp_Iir (Get_Type_Declarator (N), Sub_Indent, True);
            Header ("base_type: ", Indent);
            Disp_Iir (Get_Base_Type (N), Sub_Indent, True);
            Header ("resolved_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Resolved_Flag (N)));
            Header ("signal_type_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Signal_Type_Flag (N)));
            Header ("end_has_reserved_id: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Reserved_Id (N)));
            Header ("end_has_identifier: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Identifier (N)));
            Header ("type_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Type_Staticness (N)));
         when Iir_Kind_Record_Type_Definition =>
            Header ("elements_declaration_list: ", Indent);
            Disp_Iir_List (Get_Elements_Declaration_List (N), Sub_Indent);
            Header ("type_declarator: ", Indent);
            Disp_Iir (Get_Type_Declarator (N), Sub_Indent, True);
            Header ("base_type: ", Indent);
            Disp_Iir (Get_Base_Type (N), Sub_Indent, True);
            Header ("resolved_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Resolved_Flag (N)));
            Header ("signal_type_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Signal_Type_Flag (N)));
            Header ("has_signal_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Signal_Flag (N)));
            Header ("end_has_reserved_id: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Reserved_Id (N)));
            Header ("end_has_identifier: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Identifier (N)));
            Header ("type_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Type_Staticness (N)));
            Header ("constraint_state: ", Indent);
            Put_Line (Image_Iir_Constraint (Get_Constraint_State (N)));
         when Iir_Kind_Array_Type_Definition =>
            Header ("element_subtype_indication: ", Indent);
            Disp_Iir (Get_Element_Subtype_Indication (N), Sub_Indent);
            Header ("type_declarator: ", Indent);
            Disp_Iir (Get_Type_Declarator (N), Sub_Indent, True);
            Header ("base_type: ", Indent);
            Disp_Iir (Get_Base_Type (N), Sub_Indent, True);
            Header ("index_subtype_list: ", Indent);
            Disp_Iir_List (Get_Index_Subtype_List (N), Sub_Indent);
            Header ("resolved_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Resolved_Flag (N)));
            Header ("signal_type_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Signal_Type_Flag (N)));
            Header ("has_signal_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Signal_Flag (N)));
            Header ("index_constraint_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Index_Constraint_Flag (N)));
            Header ("type_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Type_Staticness (N)));
            Header ("constraint_state: ", Indent);
            Put_Line (Image_Iir_Constraint (Get_Constraint_State (N)));
         when Iir_Kind_Array_Subtype_Definition =>
            Header ("element_subtype_indication: ", Indent);
            Disp_Iir (Get_Element_Subtype_Indication (N), Sub_Indent);
            Header ("subtype_type_mark: ", Indent);
            Disp_Iir (Get_Subtype_Type_Mark (N), Sub_Indent);
            Header ("type_declarator: ", Indent);
            Disp_Iir (Get_Type_Declarator (N), Sub_Indent, True);
            Header ("base_type: ", Indent);
            Disp_Iir (Get_Base_Type (N), Sub_Indent, True);
            Header ("resolution_function: ", Indent);
            Disp_Iir (Get_Resolution_Function (N), Sub_Indent);
            Header ("index_subtype_list: ", Indent);
            Disp_Iir_List (Get_Index_Subtype_List (N), Sub_Indent);
            Header ("tolerance: ", Indent);
            Disp_Iir (Get_Tolerance (N), Sub_Indent);
            Header ("resolved_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Resolved_Flag (N)));
            Header ("signal_type_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Signal_Type_Flag (N)));
            Header ("has_signal_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Signal_Flag (N)));
            Header ("index_constraint_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Index_Constraint_Flag (N)));
            Header ("type_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Type_Staticness (N)));
            Header ("constraint_state: ", Indent);
            Put_Line (Image_Iir_Constraint (Get_Constraint_State (N)));
         when Iir_Kind_Record_Subtype_Definition =>
            Header ("elements_declaration_list: ", Indent);
            Disp_Iir_List (Get_Elements_Declaration_List (N), Sub_Indent);
            Header ("subtype_type_mark: ", Indent);
            Disp_Iir (Get_Subtype_Type_Mark (N), Sub_Indent);
            Header ("type_declarator: ", Indent);
            Disp_Iir (Get_Type_Declarator (N), Sub_Indent, True);
            Header ("base_type: ", Indent);
            Disp_Iir (Get_Base_Type (N), Sub_Indent, True);
            Header ("resolution_function: ", Indent);
            Disp_Iir (Get_Resolution_Function (N), Sub_Indent);
            Header ("tolerance: ", Indent);
            Disp_Iir (Get_Tolerance (N), Sub_Indent);
            Header ("resolved_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Resolved_Flag (N)));
            Header ("signal_type_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Signal_Type_Flag (N)));
            Header ("has_signal_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Signal_Flag (N)));
            Header ("type_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Type_Staticness (N)));
            Header ("constraint_state: ", Indent);
            Put_Line (Image_Iir_Constraint (Get_Constraint_State (N)));
         when Iir_Kind_Access_Subtype_Definition =>
            Header ("designated_type: ", Indent);
            Disp_Iir (Get_Designated_Type (N), Sub_Indent, True);
            Header ("subtype_type_mark: ", Indent);
            Disp_Iir (Get_Subtype_Type_Mark (N), Sub_Indent);
            Header ("type_declarator: ", Indent);
            Disp_Iir (Get_Type_Declarator (N), Sub_Indent, True);
            Header ("base_type: ", Indent);
            Disp_Iir (Get_Base_Type (N), Sub_Indent, True);
            Header ("designated_subtype_indication: ", Indent);
            Disp_Iir (Get_Designated_Subtype_Indication (N), Sub_Indent);
            Header ("resolved_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Resolved_Flag (N)));
            Header ("signal_type_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Signal_Type_Flag (N)));
            Header ("type_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Type_Staticness (N)));
         when Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition =>
            Header ("range_constraint: ", Indent);
            Disp_Iir (Get_Range_Constraint (N), Sub_Indent);
            Header ("subtype_type_mark: ", Indent);
            Disp_Iir (Get_Subtype_Type_Mark (N), Sub_Indent);
            Header ("type_declarator: ", Indent);
            Disp_Iir (Get_Type_Declarator (N), Sub_Indent, True);
            Header ("base_type: ", Indent);
            Disp_Iir (Get_Base_Type (N), Sub_Indent, True);
            Header ("resolution_function: ", Indent);
            Disp_Iir (Get_Resolution_Function (N), Sub_Indent);
            Header ("resolved_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Resolved_Flag (N)));
            Header ("signal_type_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Signal_Type_Flag (N)));
            Header ("has_signal_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Signal_Flag (N)));
            Header ("type_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Type_Staticness (N)));
         when Iir_Kind_Floating_Subtype_Definition =>
            Header ("range_constraint: ", Indent);
            Disp_Iir (Get_Range_Constraint (N), Sub_Indent);
            Header ("subtype_type_mark: ", Indent);
            Disp_Iir (Get_Subtype_Type_Mark (N), Sub_Indent);
            Header ("type_declarator: ", Indent);
            Disp_Iir (Get_Type_Declarator (N), Sub_Indent, True);
            Header ("base_type: ", Indent);
            Disp_Iir (Get_Base_Type (N), Sub_Indent, True);
            Header ("resolution_function: ", Indent);
            Disp_Iir (Get_Resolution_Function (N), Sub_Indent);
            Header ("tolerance: ", Indent);
            Disp_Iir (Get_Tolerance (N), Sub_Indent);
            Header ("resolved_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Resolved_Flag (N)));
            Header ("signal_type_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Signal_Type_Flag (N)));
            Header ("has_signal_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Signal_Flag (N)));
            Header ("type_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Type_Staticness (N)));
         when Iir_Kind_Enumeration_Type_Definition =>
            Header ("range_constraint: ", Indent);
            Disp_Iir (Get_Range_Constraint (N), Sub_Indent);
            Header ("enumeration_literal_list: ", Indent);
            Disp_Iir_List (Get_Enumeration_Literal_List (N), Sub_Indent);
            Header ("type_declarator: ", Indent);
            Disp_Iir (Get_Type_Declarator (N), Sub_Indent, True);
            Header ("base_type: ", Indent);
            Disp_Iir (Get_Base_Type (N), Sub_Indent, True);
            Header ("resolved_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Resolved_Flag (N)));
            Header ("signal_type_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Signal_Type_Flag (N)));
            Header ("has_signal_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Signal_Flag (N)));
            Header ("only_characters_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Only_Characters_Flag (N)));
            Header ("type_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Type_Staticness (N)));
         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Type_Definition =>
            Header ("type_declarator: ", Indent);
            Disp_Iir (Get_Type_Declarator (N), Sub_Indent, True);
            Header ("base_type: ", Indent);
            Disp_Iir (Get_Base_Type (N), Sub_Indent, True);
            Header ("resolved_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Resolved_Flag (N)));
            Header ("signal_type_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Signal_Type_Flag (N)));
            Header ("has_signal_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Signal_Flag (N)));
            Header ("type_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Type_Staticness (N)));
         when Iir_Kind_Physical_Type_Definition =>
            Header ("unit_chain: ", Indent);
            Disp_Chain (Get_Unit_Chain (N), Sub_Indent);
            Header ("type_declarator: ", Indent);
            Disp_Iir (Get_Type_Declarator (N), Sub_Indent, True);
            Header ("base_type: ", Indent);
            Disp_Iir (Get_Base_Type (N), Sub_Indent, True);
            Header ("resolved_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Resolved_Flag (N)));
            Header ("signal_type_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Signal_Type_Flag (N)));
            Header ("has_signal_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Signal_Flag (N)));
            Header ("end_has_reserved_id: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Reserved_Id (N)));
            Header ("end_has_identifier: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Identifier (N)));
            Header ("type_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Type_Staticness (N)));
         when Iir_Kind_Range_Expression =>
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("left_limit: ", Indent);
            Disp_Iir (Get_Left_Limit (N), Sub_Indent);
            Header ("right_limit: ", Indent);
            Disp_Iir (Get_Right_Limit (N), Sub_Indent);
            Header ("range_origin: ", Indent);
            Disp_Iir (Get_Range_Origin (N), Sub_Indent);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("direction: ", Indent);
            Put_Line (Image_Iir_Direction (Get_Direction (N)));
         when Iir_Kind_Protected_Type_Body =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("declaration_chain: ", Indent);
            Disp_Chain (Get_Declaration_Chain (N), Sub_Indent);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("protected_type_declaration: ", Indent);
            Disp_Iir (Get_Protected_Type_Declaration (N), Sub_Indent);
            Header ("end_has_reserved_id: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Reserved_Id (N)));
            Header ("end_has_identifier: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Identifier (N)));
         when Iir_Kind_Subtype_Definition =>
            Header ("range_constraint: ", Indent);
            Disp_Iir (Get_Range_Constraint (N), Sub_Indent);
            Header ("subtype_type_mark: ", Indent);
            Disp_Iir (Get_Subtype_Type_Mark (N), Sub_Indent);
            Header ("resolution_function: ", Indent);
            Disp_Iir (Get_Resolution_Function (N), Sub_Indent);
            Header ("tolerance: ", Indent);
            Disp_Iir (Get_Tolerance (N), Sub_Indent);
         when Iir_Kind_Scalar_Nature_Definition =>
            Header ("reference: ", Indent);
            Disp_Iir (Get_Reference (N), Sub_Indent);
            Header ("nature_declarator: ", Indent);
            Disp_Iir (Get_Nature_Declarator (N), Sub_Indent);
            Header ("across_type: ", Indent);
            Disp_Iir (Get_Across_Type (N), Sub_Indent);
            Header ("through_type: ", Indent);
            Disp_Iir (Get_Through_Type (N), Sub_Indent);
         when Iir_Kind_Overload_List =>
            Header ("overload_list: ", Indent);
            Disp_Iir_List (Get_Overload_List (N), Sub_Indent, True);
         when Iir_Kind_Type_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("type_definition: ", Indent);
            Disp_Iir (Get_Type_Definition (N), Sub_Indent);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
         when Iir_Kind_Anonymous_Type_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("type_definition: ", Indent);
            Disp_Iir (Get_Type_Definition (N), Sub_Indent);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("subtype_definition: ", Indent);
            Disp_Iir (Get_Subtype_Definition (N), Sub_Indent);
         when Iir_Kind_Subtype_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("subtype_indication: ", Indent);
            Disp_Iir (Get_Subtype_Indication (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
         when Iir_Kind_Nature_Declaration
           | Iir_Kind_Subnature_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("nature: ", Indent);
            Disp_Iir (Get_Nature (N), Sub_Indent);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
         when Iir_Kind_Configuration_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("declaration_chain: ", Indent);
            Disp_Chain (Get_Declaration_Chain (N), Sub_Indent);
            Header ("entity_name: ", Indent);
            Disp_Iir (Get_Entity_Name (N), Sub_Indent);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("block_configuration: ", Indent);
            Disp_Iir (Get_Block_Configuration (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("end_has_reserved_id: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Reserved_Id (N)));
            Header ("end_has_identifier: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Identifier (N)));
         when Iir_Kind_Entity_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("declaration_chain: ", Indent);
            Disp_Chain (Get_Declaration_Chain (N), Sub_Indent);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("concurrent_statement_chain: ", Indent);
            Disp_Chain (Get_Concurrent_Statement_Chain (N), Sub_Indent);
            Header ("generic_chain: ", Indent);
            Disp_Chain (Get_Generic_Chain (N), Sub_Indent);
            Header ("port_chain: ", Indent);
            Disp_Chain (Get_Port_Chain (N), Sub_Indent);
            Header ("has_begin: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Begin (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("is_within_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Is_Within_Flag (N)));
            Header ("end_has_reserved_id: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Reserved_Id (N)));
            Header ("end_has_identifier: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Identifier (N)));
         when Iir_Kind_Package_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("declaration_chain: ", Indent);
            Disp_Chain (Get_Declaration_Chain (N), Sub_Indent);
            Header ("package_body: ", Indent);
            Disp_Iir (Get_Package_Body (N), Sub_Indent);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("package_header: ", Indent);
            Disp_Iir (Get_Package_Header (N), Sub_Indent);
            Header ("need_body: ", Indent);
            Put_Line (Image_Boolean (Get_Need_Body (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("end_has_reserved_id: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Reserved_Id (N)));
            Header ("end_has_identifier: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Identifier (N)));
         when Iir_Kind_Package_Body =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("declaration_chain: ", Indent);
            Disp_Chain (Get_Declaration_Chain (N), Sub_Indent);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("package: ", Indent);
            Disp_Iir (Get_Package (N), Sub_Indent);
            Header ("end_has_reserved_id: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Reserved_Id (N)));
            Header ("end_has_identifier: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Identifier (N)));
         when Iir_Kind_Architecture_Body =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("declaration_chain: ", Indent);
            Disp_Chain (Get_Declaration_Chain (N), Sub_Indent);
            Header ("entity_name: ", Indent);
            Disp_Iir (Get_Entity_Name (N), Sub_Indent);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("concurrent_statement_chain: ", Indent);
            Disp_Chain (Get_Concurrent_Statement_Chain (N), Sub_Indent);
            Header ("default_configuration_declaration: ", Indent);
            Disp_Iir (Get_Default_Configuration_Declaration (N), Sub_Indent);
            Header ("foreign_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Foreign_Flag (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("is_within_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Is_Within_Flag (N)));
            Header ("end_has_reserved_id: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Reserved_Id (N)));
            Header ("end_has_identifier: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Identifier (N)));
         when Iir_Kind_Package_Instantiation_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("uninstantiated_name: ", Indent);
            Disp_Iir (Get_Uninstantiated_Name (N), Sub_Indent);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("generic_chain: ", Indent);
            Disp_Chain (Get_Generic_Chain (N), Sub_Indent);
            Header ("generic_map_aspect_chain: ", Indent);
            Disp_Chain (Get_Generic_Map_Aspect_Chain (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("end_has_reserved_id: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Reserved_Id (N)));
            Header ("end_has_identifier: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Identifier (N)));
         when Iir_Kind_Package_Header =>
            Header ("generic_chain: ", Indent);
            Disp_Chain (Get_Generic_Chain (N), Sub_Indent);
            Header ("generic_map_aspect_chain: ", Indent);
            Disp_Chain (Get_Generic_Map_Aspect_Chain (N), Sub_Indent);
         when Iir_Kind_Unit_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("physical_literal: ", Indent);
            Disp_Iir (Get_Physical_Literal (N), Sub_Indent);
            Header ("physical_unit_value: ", Indent);
            Disp_Iir (Get_Physical_Unit_Value (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
         when Iir_Kind_Library_Declaration =>
            Header ("design_file_chain: ", Indent);
            Disp_Chain (Get_Design_File_Chain (N), Sub_Indent);
            Header ("date: ", Indent);
            Put_Line (Date_Type'Image (Get_Date (N)));
            Header ("library_directory: ", Indent);
            Put_Line (Image_Name_Id (Get_Library_Directory (N)));
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
         when Iir_Kind_Component_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("generic_chain: ", Indent);
            Disp_Chain (Get_Generic_Chain (N), Sub_Indent);
            Header ("port_chain: ", Indent);
            Disp_Chain (Get_Port_Chain (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
            Header ("has_is: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Is (N)));
            Header ("end_has_reserved_id: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Reserved_Id (N)));
            Header ("end_has_identifier: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Identifier (N)));
         when Iir_Kind_Attribute_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("type_mark: ", Indent);
            Disp_Iir (Get_Type_Mark (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
         when Iir_Kind_Group_Template_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("entity_class_entry_chain: ", Indent);
            Disp_Chain (Get_Entity_Class_Entry_Chain (N), Sub_Indent);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
         when Iir_Kind_Group_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("group_constituent_list: ", Indent);
            Disp_Iir_List (Get_Group_Constituent_List (N), Sub_Indent);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("group_template_name: ", Indent);
            Disp_Iir (Get_Group_Template_Name (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
         when Iir_Kind_Element_Declaration =>
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("element_position: ", Indent);
            Put_Line (Iir_Index32'Image (Get_Element_Position (N)));
            Header ("subtype_indication: ", Indent);
            Disp_Iir (Get_Subtype_Indication (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("has_identifier_list: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Identifier_List (N)));
         when Iir_Kind_Non_Object_Alias_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("name: ", Indent);
            Disp_Iir (Get_Name (N), Sub_Indent);
            Header ("alias_signature: ", Indent);
            Disp_Iir (Get_Alias_Signature (N), Sub_Indent);
            Header ("implicit_alias_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Implicit_Alias_Flag (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
         when Iir_Kind_Psl_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("psl_declaration: ", Indent);
            Disp_PSL_Node (Get_Psl_Declaration (N), Sub_Indent);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("psl_clock: ", Indent);
            Disp_PSL_Node (Get_PSL_Clock (N), Sub_Indent);
            Header ("psl_nfa: ", Indent);
            Disp_PSL_NFA (Get_PSL_NFA (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
         when Iir_Kind_Terminal_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("nature: ", Indent);
            Disp_Iir (Get_Nature (N), Sub_Indent);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
         when Iir_Kind_Free_Quantity_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("default_value: ", Indent);
            Disp_Iir (Get_Default_Value (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
         when Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("default_value: ", Indent);
            Disp_Iir (Get_Default_Value (N), Sub_Indent);
            Header ("tolerance: ", Indent);
            Disp_Iir (Get_Tolerance (N), Sub_Indent);
            Header ("plus_terminal: ", Indent);
            Disp_Iir (Get_Plus_Terminal (N), Sub_Indent);
            Header ("minus_terminal: ", Indent);
            Disp_Iir (Get_Minus_Terminal (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
         when Iir_Kind_Enumeration_Literal =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("enum_pos: ", Indent);
            Put_Line (Iir_Int32'Image (Get_Enum_Pos (N)));
            Header ("subprogram_hash: ", Indent);
            Put_Line (Iir_Int32'Image (Get_Subprogram_Hash (N)));
            Header ("literal_origin: ", Indent);
            Disp_Iir (Get_Literal_Origin (N), Sub_Indent);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("enumeration_decl: ", Indent);
            Disp_Iir (Get_Enumeration_Decl (N), Sub_Indent, True);
            Header ("seen_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Seen_Flag (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("is_within_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Is_Within_Flag (N)));
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
         when Iir_Kind_Function_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("return_type: ", Indent);
            Disp_Iir (Get_Return_Type (N), Sub_Indent, True);
            Header ("subprogram_depth: ", Indent);
            Put_Line (Iir_Int32'Image (Get_Subprogram_Depth (N)));
            Header ("subprogram_hash: ", Indent);
            Put_Line (Iir_Int32'Image (Get_Subprogram_Hash (N)));
            Header ("overload_number: ", Indent);
            Put_Line (Iir_Int32'Image (Get_Overload_Number (N)));
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("interface_declaration_chain: ", Indent);
            Disp_Chain (Get_Interface_Declaration_Chain (N), Sub_Indent);
            Header ("generic_chain: ", Indent);
            Disp_Chain (Get_Generic_Chain (N), Sub_Indent);
            Header ("callees_list: ", Indent);
            Disp_Iir_List (Get_Callees_List (N), Sub_Indent);
            Header ("return_type_mark: ", Indent);
            Disp_Iir (Get_Return_Type_Mark (N), Sub_Indent);
            Header ("subprogram_body: ", Indent);
            Disp_Iir (Get_Subprogram_Body (N), Sub_Indent);
            Header ("seen_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Seen_Flag (N)));
            Header ("pure_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Pure_Flag (N)));
            Header ("foreign_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Foreign_Flag (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("is_within_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Is_Within_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
            Header ("resolution_function_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Resolution_Function_Flag (N)));
            Header ("has_pure: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Pure (N)));
            Header ("has_body: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Body (N)));
            Header ("wait_state: ", Indent);
            Put_Line (Image_Tri_State_Type (Get_Wait_State (N)));
            Header ("all_sensitized_state: ", Indent);
            Put_Line (Image_Iir_All_Sensitized (Get_All_Sensitized_State (N)));
         when Iir_Kind_Implicit_Function_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("return_type: ", Indent);
            Disp_Iir (Get_Return_Type (N), Sub_Indent, True);
            Header ("type_reference: ", Indent);
            Disp_Iir (Get_Type_Reference (N), Sub_Indent, True);
            Header ("subprogram_hash: ", Indent);
            Put_Line (Iir_Int32'Image (Get_Subprogram_Hash (N)));
            Header ("overload_number: ", Indent);
            Put_Line (Iir_Int32'Image (Get_Overload_Number (N)));
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("interface_declaration_chain: ", Indent);
            Disp_Chain (Get_Interface_Declaration_Chain (N), Sub_Indent);
            Header ("generic_chain: ", Indent);
            Disp_Chain (Get_Generic_Chain (N), Sub_Indent);
            Header ("callees_list: ", Indent);
            Disp_Iir_List (Get_Callees_List (N), Sub_Indent);
            Header ("generic_map_aspect_chain: ", Indent);
            Disp_Chain (Get_Generic_Map_Aspect_Chain (N), Sub_Indent);
            Header ("implicit_definition: ", Indent);
            Put_Line (Image_Iir_Predefined_Functions
                      (Get_Implicit_Definition (N)));
            Header ("seen_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Seen_Flag (N)));
            Header ("pure_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Pure_Flag (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("is_within_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Is_Within_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
            Header ("wait_state: ", Indent);
            Put_Line (Image_Tri_State_Type (Get_Wait_State (N)));
         when Iir_Kind_Implicit_Procedure_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("type_reference: ", Indent);
            Disp_Iir (Get_Type_Reference (N), Sub_Indent, True);
            Header ("subprogram_hash: ", Indent);
            Put_Line (Iir_Int32'Image (Get_Subprogram_Hash (N)));
            Header ("overload_number: ", Indent);
            Put_Line (Iir_Int32'Image (Get_Overload_Number (N)));
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("interface_declaration_chain: ", Indent);
            Disp_Chain (Get_Interface_Declaration_Chain (N), Sub_Indent);
            Header ("generic_chain: ", Indent);
            Disp_Chain (Get_Generic_Chain (N), Sub_Indent);
            Header ("callees_list: ", Indent);
            Disp_Iir_List (Get_Callees_List (N), Sub_Indent);
            Header ("generic_map_aspect_chain: ", Indent);
            Disp_Chain (Get_Generic_Map_Aspect_Chain (N), Sub_Indent);
            Header ("implicit_definition: ", Indent);
            Put_Line (Image_Iir_Predefined_Functions
                      (Get_Implicit_Definition (N)));
            Header ("seen_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Seen_Flag (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("is_within_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Is_Within_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
            Header ("wait_state: ", Indent);
            Put_Line (Image_Tri_State_Type (Get_Wait_State (N)));
         when Iir_Kind_Procedure_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("subprogram_depth: ", Indent);
            Put_Line (Iir_Int32'Image (Get_Subprogram_Depth (N)));
            Header ("subprogram_hash: ", Indent);
            Put_Line (Iir_Int32'Image (Get_Subprogram_Hash (N)));
            Header ("overload_number: ", Indent);
            Put_Line (Iir_Int32'Image (Get_Overload_Number (N)));
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("interface_declaration_chain: ", Indent);
            Disp_Chain (Get_Interface_Declaration_Chain (N), Sub_Indent);
            Header ("generic_chain: ", Indent);
            Disp_Chain (Get_Generic_Chain (N), Sub_Indent);
            Header ("callees_list: ", Indent);
            Disp_Iir_List (Get_Callees_List (N), Sub_Indent);
            Header ("return_type_mark: ", Indent);
            Disp_Iir (Get_Return_Type_Mark (N), Sub_Indent);
            Header ("subprogram_body: ", Indent);
            Disp_Iir (Get_Subprogram_Body (N), Sub_Indent);
            Header ("seen_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Seen_Flag (N)));
            Header ("passive_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Passive_Flag (N)));
            Header ("foreign_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Foreign_Flag (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("is_within_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Is_Within_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
            Header ("has_body: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Body (N)));
            Header ("wait_state: ", Indent);
            Put_Line (Image_Tri_State_Type (Get_Wait_State (N)));
            Header ("purity_state: ", Indent);
            Put_Line (Image_Iir_Pure_State (Get_Purity_State (N)));
            Header ("all_sensitized_state: ", Indent);
            Put_Line (Image_Iir_All_Sensitized (Get_All_Sensitized_State (N)));
         when Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("declaration_chain: ", Indent);
            Disp_Chain (Get_Declaration_Chain (N), Sub_Indent);
            Header ("impure_depth: ", Indent);
            Put_Line (Iir_Int32'Image (Get_Impure_Depth (N)));
            Header ("subprogram_specification: ", Indent);
            Disp_Iir (Get_Subprogram_Specification (N), Sub_Indent);
            Header ("sequential_statement_chain: ", Indent);
            Disp_Chain (Get_Sequential_Statement_Chain (N), Sub_Indent);
            Header ("end_has_reserved_id: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Reserved_Id (N)));
            Header ("end_has_identifier: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Identifier (N)));
         when Iir_Kind_Object_Alias_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("name: ", Indent);
            Disp_Iir (Get_Name (N), Sub_Indent);
            Header ("subtype_indication: ", Indent);
            Disp_Iir (Get_Subtype_Indication (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("after_drivers_flag: ", Indent);
            Put_Line (Image_Boolean (Get_After_Drivers_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
         when Iir_Kind_File_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("subtype_indication: ", Indent);
            Disp_Iir (Get_Subtype_Indication (N), Sub_Indent);
            Header ("file_logical_name: ", Indent);
            Disp_Iir (Get_File_Logical_Name (N), Sub_Indent);
            Header ("file_open_kind: ", Indent);
            Disp_Iir (Get_File_Open_Kind (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
            Header ("has_identifier_list: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Identifier_List (N)));
            Header ("has_mode: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Mode (N)));
            Header ("mode: ", Indent);
            Put_Line (Image_Iir_Mode (Get_Mode (N)));
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
         when Iir_Kind_Guard_Signal_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("guard_expression: ", Indent);
            Disp_Iir (Get_Guard_Expression (N), Sub_Indent);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("guard_sensitivity_list: ", Indent);
            Disp_Iir_List (Get_Guard_Sensitivity_List (N), Sub_Indent);
            Header ("block_statement: ", Indent);
            Disp_Iir (Get_Block_Statement (N), Sub_Indent);
            Header ("has_active_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Active_Flag (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
            Header ("signal_kind: ", Indent);
            Put_Line (Image_Iir_Signal_Kind (Get_Signal_Kind (N)));
         when Iir_Kind_Signal_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("subtype_indication: ", Indent);
            Disp_Iir (Get_Subtype_Indication (N), Sub_Indent);
            Header ("default_value: ", Indent);
            Disp_Iir (Get_Default_Value (N), Sub_Indent);
            Header ("signal_driver: ", Indent);
            Disp_Iir (Get_Signal_Driver (N), Sub_Indent);
            Header ("has_disconnect_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Disconnect_Flag (N)));
            Header ("has_active_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Active_Flag (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("after_drivers_flag: ", Indent);
            Put_Line (Image_Boolean (Get_After_Drivers_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
            Header ("has_identifier_list: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Identifier_List (N)));
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
            Header ("signal_kind: ", Indent);
            Put_Line (Image_Iir_Signal_Kind (Get_Signal_Kind (N)));
         when Iir_Kind_Variable_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("subtype_indication: ", Indent);
            Disp_Iir (Get_Subtype_Indication (N), Sub_Indent);
            Header ("default_value: ", Indent);
            Disp_Iir (Get_Default_Value (N), Sub_Indent);
            Header ("shared_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Shared_Flag (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
            Header ("has_identifier_list: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Identifier_List (N)));
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
         when Iir_Kind_Constant_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("subtype_indication: ", Indent);
            Disp_Iir (Get_Subtype_Indication (N), Sub_Indent);
            Header ("default_value: ", Indent);
            Disp_Iir (Get_Default_Value (N), Sub_Indent);
            Header ("deferred_declaration: ", Indent);
            Disp_Iir (Get_Deferred_Declaration (N), Sub_Indent);
            Header ("deferred_declaration_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Deferred_Declaration_Flag (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
            Header ("has_identifier_list: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Identifier_List (N)));
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
         when Iir_Kind_Iterator_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("subtype_indication: ", Indent);
            Disp_Iir (Get_Subtype_Indication (N), Sub_Indent);
            Header ("discrete_range: ", Indent);
            Disp_Iir (Get_Discrete_Range (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
            Header ("has_identifier_list: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Identifier_List (N)));
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
         when Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_File_Interface_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("subtype_indication: ", Indent);
            Disp_Iir (Get_Subtype_Indication (N), Sub_Indent);
            Header ("default_value: ", Indent);
            Disp_Iir (Get_Default_Value (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("after_drivers_flag: ", Indent);
            Put_Line (Image_Boolean (Get_After_Drivers_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
            Header ("mode: ", Indent);
            Put_Line (Image_Iir_Mode (Get_Mode (N)));
            Header ("lexical_layout: ", Indent);
            Put_Line (Image_Iir_Lexical_Layout_Type
                      (Get_Lexical_Layout (N)));
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
         when Iir_Kind_Signal_Interface_Declaration =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("subtype_indication: ", Indent);
            Disp_Iir (Get_Subtype_Indication (N), Sub_Indent);
            Header ("default_value: ", Indent);
            Disp_Iir (Get_Default_Value (N), Sub_Indent);
            Header ("has_disconnect_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Disconnect_Flag (N)));
            Header ("has_active_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Active_Flag (N)));
            Header ("open_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Open_Flag (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("after_drivers_flag: ", Indent);
            Put_Line (Image_Boolean (Get_After_Drivers_Flag (N)));
            Header ("use_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Use_Flag (N)));
            Header ("mode: ", Indent);
            Put_Line (Image_Iir_Mode (Get_Mode (N)));
            Header ("lexical_layout: ", Indent);
            Put_Line (Image_Iir_Lexical_Layout_Type
                      (Get_Lexical_Layout (N)));
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
            Header ("signal_kind: ", Indent);
            Put_Line (Image_Iir_Signal_Kind (Get_Signal_Kind (N)));
         when Iir_Kind_Identity_Operator
           | Iir_Kind_Negation_Operator
           | Iir_Kind_Absolute_Operator
           | Iir_Kind_Not_Operator
           | Iir_Kind_Condition_Operator
           | Iir_Kind_Reduction_And_Operator
           | Iir_Kind_Reduction_Or_Operator
           | Iir_Kind_Reduction_Nand_Operator
           | Iir_Kind_Reduction_Nor_Operator
           | Iir_Kind_Reduction_Xor_Operator
           | Iir_Kind_Reduction_Xnor_Operator =>
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("operand: ", Indent);
            Disp_Iir (Get_Operand (N), Sub_Indent);
            Header ("implementation: ", Indent);
            Disp_Iir (Get_Implementation (N), Sub_Indent, True);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
         when Iir_Kind_And_Operator
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
           | Iir_Kind_Exponentiation_Operator =>
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("left: ", Indent);
            Disp_Iir (Get_Left (N), Sub_Indent);
            Header ("implementation: ", Indent);
            Disp_Iir (Get_Implementation (N), Sub_Indent, True);
            Header ("right: ", Indent);
            Disp_Iir (Get_Right (N), Sub_Indent);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
         when Iir_Kind_Function_Call =>
            Header ("prefix: ", Indent);
            Disp_Iir (Get_Prefix (N), Sub_Indent);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("parameter_association_chain: ", Indent);
            Disp_Chain (Get_Parameter_Association_Chain (N), Sub_Indent);
            Header ("implementation: ", Indent);
            Disp_Iir (Get_Implementation (N), Sub_Indent, True);
            Header ("method_object: ", Indent);
            Disp_Iir (Get_Method_Object (N), Sub_Indent);
            Header ("base_name: ", Indent);
            Disp_Iir (Get_Base_Name (N), Sub_Indent, True);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
         when Iir_Kind_Aggregate =>
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("aggregate_info: ", Indent);
            Disp_Iir (Get_Aggregate_Info (N), Sub_Indent);
            Header ("association_choices_chain: ", Indent);
            Disp_Chain (Get_Association_Choices_Chain (N), Sub_Indent);
            Header ("literal_subtype: ", Indent);
            Disp_Iir (Get_Literal_Subtype (N), Sub_Indent);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("value_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Value_Staticness (N)));
         when Iir_Kind_Parenthesis_Expression =>
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("expression: ", Indent);
            Disp_Iir (Get_Expression (N), Sub_Indent);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
         when Iir_Kind_Qualified_Expression =>
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("type_mark: ", Indent);
            Disp_Iir (Get_Type_Mark (N), Sub_Indent);
            Header ("expression: ", Indent);
            Disp_Iir (Get_Expression (N), Sub_Indent);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
         when Iir_Kind_Type_Conversion =>
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("type_conversion_subtype: ", Indent);
            Disp_Iir (Get_Type_Conversion_Subtype (N), Sub_Indent);
            Header ("type_mark: ", Indent);
            Disp_Iir (Get_Type_Mark (N), Sub_Indent);
            Header ("expression: ", Indent);
            Disp_Iir (Get_Expression (N), Sub_Indent);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
         when Iir_Kind_Allocator_By_Expression =>
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("allocator_designated_type: ", Indent);
            Disp_Iir (Get_Allocator_Designated_Type (N), Sub_Indent, True);
            Header ("expression: ", Indent);
            Disp_Iir (Get_Expression (N), Sub_Indent);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
         when Iir_Kind_Allocator_By_Subtype =>
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("allocator_designated_type: ", Indent);
            Disp_Iir (Get_Allocator_Designated_Type (N), Sub_Indent, True);
            Header ("subtype_indication: ", Indent);
            Disp_Iir (Get_Subtype_Indication (N), Sub_Indent);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
         when Iir_Kind_Selected_Element =>
            Header ("prefix: ", Indent);
            Disp_Iir (Get_Prefix (N), Sub_Indent);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("selected_element: ", Indent);
            Disp_Iir (Get_Selected_Element (N), Sub_Indent);
            Header ("base_name: ", Indent);
            Disp_Iir (Get_Base_Name (N), Sub_Indent, True);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
         when Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference
           | Iir_Kind_Left_Type_Attribute
           | Iir_Kind_Right_Type_Attribute
           | Iir_Kind_High_Type_Attribute
           | Iir_Kind_Low_Type_Attribute
           | Iir_Kind_Ascending_Type_Attribute
           | Iir_Kind_Instance_Name_Attribute
           | Iir_Kind_Path_Name_Attribute =>
            Header ("prefix: ", Indent);
            Disp_Iir (Get_Prefix (N), Sub_Indent);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("base_name: ", Indent);
            Disp_Iir (Get_Base_Name (N), Sub_Indent, True);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
         when Iir_Kind_Slice_Name =>
            Header ("prefix: ", Indent);
            Disp_Iir (Get_Prefix (N), Sub_Indent);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("suffix: ", Indent);
            Disp_Iir (Get_Suffix (N), Sub_Indent);
            Header ("slice_subtype: ", Indent);
            Disp_Iir (Get_Slice_Subtype (N), Sub_Indent);
            Header ("base_name: ", Indent);
            Disp_Iir (Get_Base_Name (N), Sub_Indent, True);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
         when Iir_Kind_Indexed_Name =>
            Header ("prefix: ", Indent);
            Disp_Iir (Get_Prefix (N), Sub_Indent);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("index_list: ", Indent);
            Disp_Iir_List (Get_Index_List (N), Sub_Indent);
            Header ("base_name: ", Indent);
            Disp_Iir (Get_Base_Name (N), Sub_Indent, True);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
         when Iir_Kind_Psl_Expression =>
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("psl_expression: ", Indent);
            Disp_PSL_Node (Get_Psl_Expression (N), Sub_Indent);
         when Iir_Kind_Sensitized_Process_Statement =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("declaration_chain: ", Indent);
            Disp_Chain (Get_Declaration_Chain (N), Sub_Indent);
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("sequential_statement_chain: ", Indent);
            Disp_Chain (Get_Sequential_Statement_Chain (N), Sub_Indent);
            Header ("sensitivity_list: ", Indent);
            Disp_Iir_List (Get_Sensitivity_List (N), Sub_Indent);
            Header ("callees_list: ", Indent);
            Disp_Iir_List (Get_Callees_List (N), Sub_Indent);
            Header ("process_origin: ", Indent);
            Disp_Iir (Get_Process_Origin (N), Sub_Indent);
            Header ("seen_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Seen_Flag (N)));
            Header ("end_has_postponed: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Postponed (N)));
            Header ("passive_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Passive_Flag (N)));
            Header ("postponed_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Postponed_Flag (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("is_within_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Is_Within_Flag (N)));
            Header ("has_is: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Is (N)));
            Header ("end_has_reserved_id: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Reserved_Id (N)));
            Header ("end_has_identifier: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Identifier (N)));
            Header ("wait_state: ", Indent);
            Put_Line (Image_Tri_State_Type (Get_Wait_State (N)));
         when Iir_Kind_Process_Statement =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("declaration_chain: ", Indent);
            Disp_Chain (Get_Declaration_Chain (N), Sub_Indent);
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("sequential_statement_chain: ", Indent);
            Disp_Chain (Get_Sequential_Statement_Chain (N), Sub_Indent);
            Header ("callees_list: ", Indent);
            Disp_Iir_List (Get_Callees_List (N), Sub_Indent);
            Header ("process_origin: ", Indent);
            Disp_Iir (Get_Process_Origin (N), Sub_Indent);
            Header ("seen_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Seen_Flag (N)));
            Header ("end_has_postponed: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Postponed (N)));
            Header ("passive_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Passive_Flag (N)));
            Header ("postponed_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Postponed_Flag (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("is_within_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Is_Within_Flag (N)));
            Header ("has_is: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Is (N)));
            Header ("end_has_reserved_id: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Reserved_Id (N)));
            Header ("end_has_identifier: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Identifier (N)));
            Header ("wait_state: ", Indent);
            Put_Line (Image_Tri_State_Type (Get_Wait_State (N)));
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("target: ", Indent);
            Disp_Iir (Get_Target (N), Sub_Indent);
            Header ("delay_mechanism: ", Indent);
            Put_Line (Image_Iir_Delay_Mechanism (Get_Delay_Mechanism (N)));
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("reject_time_expression: ", Indent);
            Disp_Iir (Get_Reject_Time_Expression (N), Sub_Indent);
            Header ("conditional_waveform_chain: ", Indent);
            Disp_Chain (Get_Conditional_Waveform_Chain (N), Sub_Indent);
            Header ("guard: ", Indent);
            Disp_Iir (Get_Guard (N), Sub_Indent);
            Header ("postponed_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Postponed_Flag (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("guarded_target_state: ", Indent);
            Put_Line (Image_Tri_State_Type (Get_Guarded_Target_State (N)));
         when Iir_Kind_Concurrent_Selected_Signal_Assignment =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("target: ", Indent);
            Disp_Iir (Get_Target (N), Sub_Indent);
            Header ("delay_mechanism: ", Indent);
            Put_Line (Image_Iir_Delay_Mechanism (Get_Delay_Mechanism (N)));
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("expression: ", Indent);
            Disp_Iir (Get_Expression (N), Sub_Indent);
            Header ("reject_time_expression: ", Indent);
            Disp_Iir (Get_Reject_Time_Expression (N), Sub_Indent);
            Header ("selected_waveform_chain: ", Indent);
            Disp_Chain (Get_Selected_Waveform_Chain (N), Sub_Indent);
            Header ("guard: ", Indent);
            Disp_Iir (Get_Guard (N), Sub_Indent);
            Header ("postponed_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Postponed_Flag (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("guarded_target_state: ", Indent);
            Put_Line (Image_Tri_State_Type (Get_Guarded_Target_State (N)));
         when Iir_Kind_Concurrent_Assertion_Statement =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("assertion_condition: ", Indent);
            Disp_Iir (Get_Assertion_Condition (N), Sub_Indent);
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("severity_expression: ", Indent);
            Disp_Iir (Get_Severity_Expression (N), Sub_Indent);
            Header ("report_expression: ", Indent);
            Disp_Iir (Get_Report_Expression (N), Sub_Indent);
            Header ("postponed_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Postponed_Flag (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
         when Iir_Kind_Psl_Default_Clock =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("psl_boolean: ", Indent);
            Disp_PSL_Node (Get_Psl_Boolean (N), Sub_Indent);
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
         when Iir_Kind_Psl_Assert_Statement
           | Iir_Kind_Psl_Cover_Statement =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("psl_property: ", Indent);
            Disp_PSL_Node (Get_Psl_Property (N), Sub_Indent);
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("severity_expression: ", Indent);
            Disp_Iir (Get_Severity_Expression (N), Sub_Indent);
            Header ("report_expression: ", Indent);
            Disp_Iir (Get_Report_Expression (N), Sub_Indent);
            Header ("psl_clock: ", Indent);
            Disp_PSL_Node (Get_PSL_Clock (N), Sub_Indent);
            Header ("psl_nfa: ", Indent);
            Disp_PSL_NFA (Get_PSL_NFA (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
         when Iir_Kind_Concurrent_Procedure_Call_Statement =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("procedure_call: ", Indent);
            Disp_Iir (Get_Procedure_Call (N), Sub_Indent);
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("postponed_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Postponed_Flag (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
         when Iir_Kind_Block_Statement =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("declaration_chain: ", Indent);
            Disp_Chain (Get_Declaration_Chain (N), Sub_Indent);
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("concurrent_statement_chain: ", Indent);
            Disp_Chain (Get_Concurrent_Statement_Chain (N), Sub_Indent);
            Header ("block_block_configuration: ", Indent);
            Disp_Iir (Get_Block_Block_Configuration (N), Sub_Indent);
            Header ("block_header: ", Indent);
            Disp_Iir (Get_Block_Header (N), Sub_Indent);
            Header ("guard_decl: ", Indent);
            Disp_Iir (Get_Guard_Decl (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("is_within_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Is_Within_Flag (N)));
            Header ("end_has_reserved_id: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Reserved_Id (N)));
            Header ("end_has_identifier: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Identifier (N)));
         when Iir_Kind_Generate_Statement =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("declaration_chain: ", Indent);
            Disp_Chain (Get_Declaration_Chain (N), Sub_Indent);
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("concurrent_statement_chain: ", Indent);
            Disp_Chain (Get_Concurrent_Statement_Chain (N), Sub_Indent);
            Header ("generation_scheme: ", Indent);
            Disp_Iir (Get_Generation_Scheme (N), Sub_Indent);
            Header ("generate_block_configuration: ", Indent);
            Disp_Iir (Get_Generate_Block_Configuration (N), Sub_Indent);
            Header ("has_begin: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Begin (N)));
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("end_has_reserved_id: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Reserved_Id (N)));
            Header ("end_has_identifier: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Identifier (N)));
         when Iir_Kind_Component_Instantiation_Statement =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("instantiated_unit: ", Indent);
            Disp_Iir (Get_Instantiated_Unit (N), Sub_Indent);
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("default_binding_indication: ", Indent);
            Disp_Iir (Get_Default_Binding_Indication (N), Sub_Indent);
            Header ("component_configuration: ", Indent);
            Disp_Iir (Get_Component_Configuration (N), Sub_Indent);
            Header ("configuration_specification: ", Indent);
            Disp_Iir (Get_Configuration_Specification (N), Sub_Indent);
            Header ("generic_map_aspect_chain: ", Indent);
            Disp_Chain (Get_Generic_Map_Aspect_Chain (N), Sub_Indent);
            Header ("port_map_aspect_chain: ", Indent);
            Disp_Chain (Get_Port_Map_Aspect_Chain (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
         when Iir_Kind_Simple_Simultaneous_Statement =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("simultaneous_left: ", Indent);
            Disp_Iir (Get_Simultaneous_Left (N), Sub_Indent);
            Header ("simultaneous_right: ", Indent);
            Disp_Iir (Get_Simultaneous_Right (N), Sub_Indent);
            Header ("tolerance: ", Indent);
            Disp_Iir (Get_Tolerance (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
         when Iir_Kind_Signal_Assignment_Statement =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("target: ", Indent);
            Disp_Iir (Get_Target (N), Sub_Indent);
            Header ("delay_mechanism: ", Indent);
            Put_Line (Image_Iir_Delay_Mechanism (Get_Delay_Mechanism (N)));
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("waveform_chain: ", Indent);
            Disp_Chain (Get_Waveform_Chain (N), Sub_Indent);
            Header ("reject_time_expression: ", Indent);
            Disp_Iir (Get_Reject_Time_Expression (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("guarded_target_state: ", Indent);
            Put_Line (Image_Tri_State_Type (Get_Guarded_Target_State (N)));
         when Iir_Kind_Null_Statement =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
         when Iir_Kind_Assertion_Statement =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("assertion_condition: ", Indent);
            Disp_Iir (Get_Assertion_Condition (N), Sub_Indent);
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("severity_expression: ", Indent);
            Disp_Iir (Get_Severity_Expression (N), Sub_Indent);
            Header ("report_expression: ", Indent);
            Disp_Iir (Get_Report_Expression (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
         when Iir_Kind_Report_Statement =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("severity_expression: ", Indent);
            Disp_Iir (Get_Severity_Expression (N), Sub_Indent);
            Header ("report_expression: ", Indent);
            Disp_Iir (Get_Report_Expression (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
         when Iir_Kind_Wait_Statement =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("timeout_clause: ", Indent);
            Disp_Iir (Get_Timeout_Clause (N), Sub_Indent);
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("condition_clause: ", Indent);
            Disp_Iir (Get_Condition_Clause (N), Sub_Indent);
            Header ("sensitivity_list: ", Indent);
            Disp_Iir_List (Get_Sensitivity_List (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
         when Iir_Kind_Variable_Assignment_Statement =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("target: ", Indent);
            Disp_Iir (Get_Target (N), Sub_Indent);
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("expression: ", Indent);
            Disp_Iir (Get_Expression (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
         when Iir_Kind_Return_Statement =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("expression: ", Indent);
            Disp_Iir (Get_Expression (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
         when Iir_Kind_For_Loop_Statement =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("parameter_specification: ", Indent);
            Disp_Iir (Get_Parameter_Specification (N), Sub_Indent);
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("sequential_statement_chain: ", Indent);
            Disp_Chain (Get_Sequential_Statement_Chain (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("is_within_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Is_Within_Flag (N)));
            Header ("end_has_identifier: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Identifier (N)));
         when Iir_Kind_While_Loop_Statement =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("condition: ", Indent);
            Disp_Iir (Get_Condition (N), Sub_Indent);
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("sequential_statement_chain: ", Indent);
            Disp_Chain (Get_Sequential_Statement_Chain (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("end_has_identifier: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Identifier (N)));
         when Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("condition: ", Indent);
            Disp_Iir (Get_Condition (N), Sub_Indent);
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("loop_label: ", Indent);
            Disp_Iir (Get_Loop_Label (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
         when Iir_Kind_Case_Statement =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("case_statement_alternative_chain: ", Indent);
            Disp_Chain (Get_Case_Statement_Alternative_Chain (N), Sub_Indent);
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("expression: ", Indent);
            Disp_Iir (Get_Expression (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("end_has_identifier: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Identifier (N)));
         when Iir_Kind_Procedure_Call_Statement =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("procedure_call: ", Indent);
            Disp_Iir (Get_Procedure_Call (N), Sub_Indent);
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
         when Iir_Kind_If_Statement =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("condition: ", Indent);
            Disp_Iir (Get_Condition (N), Sub_Indent);
            Header ("label: ", Indent);
            Put_Line (Image_Name_Id (Get_Label (N)));
            Header ("attribute_value_chain: ", Indent);
            Disp_Iir (Get_Attribute_Value_Chain (N), Sub_Indent);
            Header ("sequential_statement_chain: ", Indent);
            Disp_Chain (Get_Sequential_Statement_Chain (N), Sub_Indent);
            Header ("else_clause: ", Indent);
            Disp_Iir (Get_Else_Clause (N), Sub_Indent);
            Header ("visible_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Visible_Flag (N)));
            Header ("end_has_identifier: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Identifier (N)));
         when Iir_Kind_Elsif =>
            Header ("parent: ", Indent);
            Disp_Iir (Get_Parent (N), Sub_Indent, True);
            Header ("condition: ", Indent);
            Disp_Iir (Get_Condition (N), Sub_Indent);
            Header ("sequential_statement_chain: ", Indent);
            Disp_Chain (Get_Sequential_Statement_Chain (N), Sub_Indent);
            Header ("else_clause: ", Indent);
            Disp_Iir (Get_Else_Clause (N), Sub_Indent);
            Header ("end_has_identifier: ", Indent);
            Put_Line (Image_Boolean (Get_End_Has_Identifier (N)));
         when Iir_Kind_Character_Literal
           | Iir_Kind_Simple_Name =>
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("alias_declaration: ", Indent);
            Disp_Iir (Get_Alias_Declaration (N), Sub_Indent);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("named_entity: ", Indent);
            Disp_Iir (Get_Named_Entity (N), Sub_Indent, True);
            Header ("base_name: ", Indent);
            Disp_Iir (Get_Base_Name (N), Sub_Indent, True);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
         when Iir_Kind_Selected_Name =>
            Header ("prefix: ", Indent);
            Disp_Iir (Get_Prefix (N), Sub_Indent);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("alias_declaration: ", Indent);
            Disp_Iir (Get_Alias_Declaration (N), Sub_Indent);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("named_entity: ", Indent);
            Disp_Iir (Get_Named_Entity (N), Sub_Indent, True);
            Header ("base_name: ", Indent);
            Disp_Iir (Get_Base_Name (N), Sub_Indent, True);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
         when Iir_Kind_Operator_Symbol =>
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("alias_declaration: ", Indent);
            Disp_Iir (Get_Alias_Declaration (N), Sub_Indent);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("named_entity: ", Indent);
            Disp_Iir (Get_Named_Entity (N), Sub_Indent, True);
            Header ("base_name: ", Indent);
            Disp_Iir (Get_Base_Name (N), Sub_Indent, True);
         when Iir_Kind_Selected_By_All_Name =>
            Header ("prefix: ", Indent);
            Disp_Iir (Get_Prefix (N), Sub_Indent);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("named_entity: ", Indent);
            Disp_Iir (Get_Named_Entity (N), Sub_Indent, True);
            Header ("base_name: ", Indent);
            Disp_Iir (Get_Base_Name (N), Sub_Indent, True);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
         when Iir_Kind_Parenthesis_Name =>
            Header ("prefix: ", Indent);
            Disp_Iir (Get_Prefix (N), Sub_Indent);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("association_chain: ", Indent);
            Disp_Chain (Get_Association_Chain (N), Sub_Indent);
            Header ("named_entity: ", Indent);
            Disp_Iir (Get_Named_Entity (N), Sub_Indent, True);
         when Iir_Kind_Base_Attribute =>
            Header ("prefix: ", Indent);
            Disp_Iir (Get_Prefix (N), Sub_Indent);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
         when Iir_Kind_Image_Attribute
           | Iir_Kind_Value_Attribute
           | Iir_Kind_Pos_Attribute
           | Iir_Kind_Val_Attribute
           | Iir_Kind_Succ_Attribute
           | Iir_Kind_Pred_Attribute
           | Iir_Kind_Leftof_Attribute
           | Iir_Kind_Rightof_Attribute =>
            Header ("prefix: ", Indent);
            Disp_Iir (Get_Prefix (N), Sub_Indent);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("parameter: ", Indent);
            Disp_Iir (Get_Parameter (N), Sub_Indent);
            Header ("base_name: ", Indent);
            Disp_Iir (Get_Base_Name (N), Sub_Indent, True);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
         when Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute =>
            Header ("prefix: ", Indent);
            Disp_Iir (Get_Prefix (N), Sub_Indent);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("parameter: ", Indent);
            Disp_Iir (Get_Parameter (N), Sub_Indent);
            Header ("base_name: ", Indent);
            Disp_Iir (Get_Base_Name (N), Sub_Indent, True);
            Header ("has_active_flag: ", Indent);
            Put_Line (Image_Boolean (Get_Has_Active_Flag (N)));
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
         when Iir_Kind_Event_Attribute
           | Iir_Kind_Active_Attribute
           | Iir_Kind_Last_Event_Attribute
           | Iir_Kind_Last_Active_Attribute
           | Iir_Kind_Last_Value_Attribute
           | Iir_Kind_Driving_Attribute
           | Iir_Kind_Driving_Value_Attribute =>
            Header ("prefix: ", Indent);
            Disp_Iir (Get_Prefix (N), Sub_Indent);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
         when Iir_Kind_Simple_Name_Attribute =>
            Header ("prefix: ", Indent);
            Disp_Iir (Get_Prefix (N), Sub_Indent);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("simple_name_identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Simple_Name_Identifier (N)));
            Header ("simple_name_subtype: ", Indent);
            Disp_Iir (Get_Simple_Name_Subtype (N), Sub_Indent);
            Header ("base_name: ", Indent);
            Disp_Iir (Get_Base_Name (N), Sub_Indent, True);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
         when Iir_Kind_Left_Array_Attribute
           | Iir_Kind_Right_Array_Attribute
           | Iir_Kind_High_Array_Attribute
           | Iir_Kind_Low_Array_Attribute
           | Iir_Kind_Length_Array_Attribute
           | Iir_Kind_Ascending_Array_Attribute
           | Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute =>
            Header ("prefix: ", Indent);
            Disp_Iir (Get_Prefix (N), Sub_Indent);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("index_subtype: ", Indent);
            Disp_Iir (Get_Index_Subtype (N), Sub_Indent);
            Header ("parameter: ", Indent);
            Disp_Iir (Get_Parameter (N), Sub_Indent);
            Header ("base_name: ", Indent);
            Disp_Iir (Get_Base_Name (N), Sub_Indent, True);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
         when Iir_Kind_Attribute_Name =>
            Header ("prefix: ", Indent);
            Disp_Iir (Get_Prefix (N), Sub_Indent);
            Header ("type: ", Indent);
            Disp_Iir (Get_Type (N), Sub_Indent, True);
            Header ("attribute_signature: ", Indent);
            Disp_Iir (Get_Attribute_Signature (N), Sub_Indent);
            Header ("identifier: ", Indent);
            Put_Line (Image_Name_Id (Get_Identifier (N)));
            Header ("named_entity: ", Indent);
            Disp_Iir (Get_Named_Entity (N), Sub_Indent, True);
            Header ("base_name: ", Indent);
            Disp_Iir (Get_Base_Name (N), Sub_Indent, True);
            Header ("expr_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Expr_Staticness (N)));
            Header ("name_staticness: ", Indent);
            Put_Line (Image_Iir_Staticness (Get_Name_Staticness (N)));
      end case;
   end Disp_Iir;


   procedure Disp_Tree_For_Psl (N : Int32) is
   begin
      Disp_Tree_Flat (Iir (N), 1);
   end Disp_Tree_For_Psl;

   procedure Disp_Tree (Tree : Iir;
                        Flat : Boolean := false) is
   begin
      Disp_Iir (Tree, 1, Flat);
   end Disp_Tree;
end Disp_Tree;
