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
with Ada.Text_IO; use Ada.Text_IO;
with Name_Table;
with Iirs_Utils; use Iirs_Utils;
with Tokens;
with Errorout;
with Files_Map;
with PSL.Dump_Tree;

package body Disp_Tree is
   procedure Disp_Tab (Tab: Natural) is
      Blanks : constant String (1 .. Tab) := (others => ' ');
   begin
      Put (Blanks);
   end Disp_Tab;

   procedure Disp_Iir_Address (Node: Iir)
   is
      Res : String (1 .. 10);
      Hex_Digits : constant array (Int32 range 0 .. 15) of Character
        := "0123456789abcdef";
      N : Int32 := Int32 (Node);
   begin
      for I in reverse 2 .. 9 loop
         Res (I) := Hex_Digits (N mod 16);
         N := N / 16;
      end loop;
      Res (1) := '[';
      Res (10) := ']';
      Put (Res);
   end Disp_Iir_Address;

   function Inc_Tab (Tab: Natural) return Natural is
   begin
      return Tab + 4;
   end Inc_Tab;


   -- For iir.

   procedure Disp_Tree_Flat (Tree: Iir; Tab: Natural);

   procedure Disp_Tree_List
     (Tree_List: Iir_List; Tab: Natural; Flat_Decl : Boolean := False)
   is
      El: Iir;
   begin
      if Tree_List = Null_Iir_List then
         Disp_Tab (Tab);
         Put_Line (" null-list");
      elsif Tree_List = Iir_List_All then
         Disp_Tab (Tab);
         Put_Line (" list-all");
      elsif Tree_List = Iir_List_Others then
         Disp_Tab (Tab);
         Put_Line (" list-others");
      else
         for I in Natural loop
            El := Get_Nth_Element (Tree_List, I);
            exit when El = Null_Iir;
            Disp_Tree (El, Tab, Flat_Decl);
         end loop;
      end if;
   end Disp_Tree_List;

   procedure Disp_Tree_Chain
     (Tree_Chain: Iir; Tab: Natural; Flat_Decl : Boolean := False)
   is
      El: Iir;
   begin
      El := Tree_Chain;
      while El /= Null_Iir loop
         Disp_Tree (El, Tab, Flat_Decl);
         El := Get_Chain (El);
      end loop;
   end Disp_Tree_Chain;

   procedure Disp_Tree_Flat_Chain (Tree_Chain: Iir; Tab: Natural)
   is
      El: Iir;
   begin
      El := Tree_Chain;
      while El /= Null_Iir loop
         Disp_Tree_Flat (El, Tab);
         El := Get_Chain (El);
      end loop;
   end Disp_Tree_Flat_Chain;

   procedure Disp_Tree_List_Flat (Tree_List: Iir_List; Tab: Natural)
   is
      El: Iir;
   begin
      if Tree_List = Null_Iir_List then
         Disp_Tab (Tab);
         Put_Line (" null-list");
      elsif Tree_List = Iir_List_All then
         Disp_Tab (Tab);
         Put_Line (" list-all");
      elsif Tree_List = Iir_List_Others then
         Disp_Tab (Tab);
         Put_Line (" list-others");
      else
         for I in Natural loop
            El := Get_Nth_Element (Tree_List, I);
            exit when El = Null_Iir;
            Disp_Tree_Flat (El, Tab);
         end loop;
      end if;
   end Disp_Tree_List_Flat;

   procedure Disp_Ident (Ident: Name_Id)
   is
      use Name_Table;
   begin
      if Ident /= Null_Identifier then
         Image (Ident);
         Put_Line (" '" & Name_Buffer (1 .. Name_Length) & ''');
      else
         Put_Line (" <anonymous>");
      end if;
   end Disp_Ident;

   procedure Disp_Tree_Flat (Tree: Iir; Tab: Natural)
   is
      procedure Disp_Identifier (Identifying: Iir)
      is
         Ident : Name_Id;
      begin
         if Identifying /= Null_Iir then
            Ident := Get_Identifier (Identifying);
            Disp_Ident (Ident);
         else
            New_Line;
         end if;
      end Disp_Identifier;

      procedure Disp_Decl_Ident
      is
         A_Type: Iir;
      begin
         A_Type := Get_Type_Declarator (Tree);
         if A_Type /= Null_Iir then
            Disp_Identifier (A_Type);
         else
            Put_Line (" <unnamed>");
            return;
         end if;
      end Disp_Decl_Ident;
   begin
      Disp_Tab (Tab);
      Disp_Iir_Address (Tree);

      if Tree = Null_Iir then
         Put_Line (" *NULL*");
         return;
      else
         Put (' ');
      end if;

      case Get_Kind (Tree) is
         when Iir_Kind_Design_File =>
            Put_Line ("design file");

         when Iir_Kind_Design_Unit =>
            Put ("design_unit");
            Disp_Identifier (Tree);

         when Iir_Kind_Use_Clause =>
            Put_Line ("use_clause");

         when Iir_Kind_Library_Clause =>
            Put ("library clause");
            Disp_Identifier (Tree);

         when Iir_Kind_Library_Declaration =>
            Put ("library declaration");
            Disp_Identifier (Tree);

         when Iir_Kind_Proxy =>
            Put_Line ("proxy");

         when Iir_Kind_Waveform_Element =>
            Put_Line ("waveform_element");

         when Iir_Kind_Package_Declaration =>
            Put ("package_declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_Package_Body =>
            Put ("package_body");
            Disp_Identifier (Tree);
         when Iir_Kind_Entity_Declaration =>
            Put ("entity_declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_Architecture_Declaration =>
            Put ("architecture_declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_Configuration_Declaration =>
            Put ("configuration_declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_Function_Declaration =>
            Put ("function_declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_Function_Body =>
            Put_Line ("function_body");
         when Iir_Kind_Procedure_Declaration =>
            Put ("procedure_declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_Procedure_Body =>
            Put_Line ("procedure_body");
         when Iir_Kind_Object_Alias_Declaration =>
            Put ("object_alias_declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_Non_Object_Alias_Declaration =>
            Put ("non_object_alias_declaration");
            Disp_Identifier (Tree);

         when Iir_Kind_Signal_Interface_Declaration =>
            Put ("signal_interface_declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_Signal_Declaration =>
            Put ("signal_declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_Variable_Interface_Declaration =>
            Put ("variable_interface_declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_Variable_Declaration =>
            if Get_Shared_Flag (Tree) then
               Put ("(shared) ");
            end if;
            Put ("variable_declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_Constant_Interface_Declaration =>
            Put ("constant_interface_declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_Constant_Declaration =>
            Put ("constant_declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_Iterator_Declaration =>
            Put ("iterator_declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_File_Interface_Declaration =>
            Put ("file_interface_declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_File_Declaration =>
            Put ("file_declaration");
            Disp_Identifier (Tree);

         when Iir_Kind_Type_Declaration =>
            Put ("type_declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_Anonymous_Type_Declaration =>
            Put ("anonymous_type_declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_Subtype_Declaration =>
            Put ("subtype_declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_Component_Declaration =>
            Put ("component_declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_Element_Declaration =>
            Put ("element_declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_Record_Element_Constraint =>
            Put ("record_element_constraint");
            Disp_Identifier (Tree);
         when Iir_Kind_Attribute_Declaration =>
            Put ("attribute_declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_Group_Template_Declaration =>
            Put ("group_template_declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_Group_Declaration =>
            Put ("group_declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_Psl_Declaration =>
            Put ("psl declaration");
            Disp_Identifier (Tree);
         when Iir_Kind_Psl_Expression =>
            Put ("psl expression");

         when Iir_Kind_Enumeration_Type_Definition =>
            Put ("enumeration_type_definition");
            Disp_Decl_Ident;
         when Iir_Kind_Enumeration_Subtype_Definition =>
            Put ("enumeration_subtype_definition");
            Disp_Decl_Ident;
         when Iir_Kind_Integer_Subtype_Definition =>
            Put ("integer_subtype_definition");
            Disp_Decl_Ident;
         when Iir_Kind_Integer_Type_Definition =>
            Put ("integer_type_definition");
            Disp_Identifier (Get_Type_Declarator (Tree));
         when Iir_Kind_Floating_Subtype_Definition =>
            Put ("floating_subtype_definition");
            Disp_Decl_Ident;
         when Iir_Kind_Floating_Type_Definition =>
            Put ("floating_type_definition");
            Disp_Identifier (Get_Type_Declarator (Tree));
         when Iir_Kind_Array_Subtype_Definition =>
            Put ("array_subtype_definition");
            Disp_Decl_Ident;
         when Iir_Kind_Array_Type_Definition =>
            Put ("array_type_definition");
            Disp_Decl_Ident;
         when Iir_Kind_Record_Type_Definition =>
            Put ("record_type_definition");
            Disp_Decl_Ident;
         when Iir_Kind_Access_Type_Definition =>
            Put ("access_type_definition");
            Disp_Decl_Ident;
         when Iir_Kind_File_Type_Definition =>
            Put ("file_type_definition");
            Disp_Identifier (Get_Type_Declarator (Tree));
         when Iir_Kind_Subtype_Definition =>
            Put_Line ("subtype_definition");
         when Iir_Kind_Physical_Type_Definition =>
            Put ("physical_type_definition");
            Disp_Identifier (Get_Type_Declarator (Tree));
         when Iir_Kind_Physical_Subtype_Definition =>
            Put_Line ("physical_subtype_definition");

         when Iir_Kind_Simple_Name =>
            Put ("simple_name ");
            Disp_Identifier (Tree);

         when Iir_Kind_Operator_Symbol =>
            Put ("operator_symbol """);
            Name_Table.Image (Get_Identifier (Tree));
            Put (Name_Table.Name_Buffer (1 .. Name_Table.Name_Length));
            Put_Line ("""");

         when Iir_Kind_Null_Literal =>
            Put_Line ("null_literal");

         when Iir_Kind_Physical_Int_Literal =>
            Put_Line ("physical_int_literal");

         when Iir_Kind_Physical_Fp_Literal =>
            Put_Line ("physical_fp_literal");

         when Iir_Kind_Component_Instantiation_Statement =>
            Put ("component_instantiation_statement");
            Disp_Ident (Get_Label (Tree));
         when Iir_Kind_Block_Statement =>
            Put ("block_statement");
            Disp_Ident (Get_Label (Tree));
         when Iir_Kind_Sensitized_Process_Statement =>
            Put ("sensitized_process_statement");
            Disp_Ident (Get_Label (Tree));
         when Iir_Kind_Process_Statement =>
            Put ("process_statement");
            Disp_Ident (Get_Label (Tree));
         when Iir_Kind_Case_Statement =>
            Put_Line ("case_statement");
         when Iir_Kind_If_Statement =>
            Put_Line ("if_statement");
         when Iir_Kind_Elsif =>
            Put_Line ("Elsif");
         when Iir_Kind_For_Loop_Statement =>
            Put_Line ("for_loop_statement");
         when Iir_Kind_While_Loop_Statement =>
            Put_Line ("while_loop_statement");
         when Iir_Kind_Exit_Statement =>
            Put_Line ("exit_statement");
         when Iir_Kind_Next_Statement =>
            Put_Line ("next_statement");
         when Iir_Kind_Wait_Statement =>
            Put_Line ("wait_statement");
         when Iir_Kind_Assertion_Statement =>
            Put_Line ("assertion_statement");
         when Iir_Kind_Variable_Assignment_Statement =>
            Put_Line ("variable_assignment_statement");
         when Iir_Kind_Signal_Assignment_Statement =>
            Put_Line ("signal_assignment_statement");
         when Iir_Kind_Concurrent_Assertion_Statement =>
            Put_Line ("concurrent_assertion_statement");
         when Iir_Kind_Procedure_Call_Statement =>
            Put_Line ("procedure_call_statement");
         when Iir_Kind_Concurrent_Procedure_Call_Statement =>
            Put_Line ("concurrent_procedure_call_statement");
         when Iir_Kind_Return_Statement =>
            Put_Line ("return_statement");
         when Iir_Kind_Null_Statement =>
            Put_Line ("null_statement");

         when Iir_Kind_Enumeration_Literal =>
            Put ("enumeration_literal");
            Disp_Identifier (Tree);

         when Iir_Kind_Character_Literal =>
            Put_Line ("character_literal");
         when Iir_Kind_Integer_Literal =>
            Put_Line ("integer_literal: "
                      & Iir_Int64'Image (Get_Value (Tree)));
         when Iir_Kind_Floating_Point_Literal =>
            Put_Line ("floating_point_literal: "
                      & Iir_Fp64'Image (Get_Fp_Value (Tree)));
         when Iir_Kind_String_Literal =>
            Put_Line ("string_literal: " & Image_String_Lit (Tree));
         when Iir_Kind_Unit_Declaration =>
            Put ("physical unit");
            Disp_Identifier (Tree);
         when Iir_Kind_Entity_Class =>
            Put_Line ("entity_class '"
                      & Tokens.Image (Get_Entity_Class (Tree)) & ''');

         when Iir_Kind_Attribute_Name =>
            Put ("attribute_name");
            Disp_Ident (Get_Attribute_Identifier (Tree));

         when Iir_Kind_Implicit_Function_Declaration =>
            Put ("implicit_function_declaration: ");
            Put_Line (Iirs_Utils.Get_Predefined_Function_Name
                      (Get_Implicit_Definition (Tree)));
         when Iir_Kind_Implicit_Procedure_Declaration =>
            Put ("implicit_procedure_declaration: ");
            Put_Line (Iirs_Utils.Get_Predefined_Function_Name
                      (Get_Implicit_Definition (Tree)));

         when others =>
            Put_Line (Iir_Kind'Image (Get_Kind (Tree)));
      end case;
   end Disp_Tree_Flat;

   procedure Disp_Staticness (Static: Iir_Staticness) is
   begin
      case Static is
         when Unknown =>
            Put ("???");
         when None =>
            Put ("none");
         when Globally =>
            Put ("global");
         when Locally =>
            Put ("local");
      end case;
   end Disp_Staticness;

   procedure Disp_Flag (Bool : Boolean) is
   begin
      if Bool then
         Put ("true");
      else
         Put ("false");
      end if;
      New_Line;
   end Disp_Flag;

   procedure Disp_Expr_Staticness (Expr: Iir) is
   begin
      Put (" expr: ");
      Disp_Staticness (Get_Expr_Staticness (Expr));
      New_Line;
   end Disp_Expr_Staticness;

   procedure Disp_Type_Staticness (Atype: Iir) is
   begin
      Put (" type: ");
      Disp_Staticness (Get_Type_Staticness (Atype));
      New_Line;
   end Disp_Type_Staticness;

   procedure Disp_Name_Staticness (Expr: Iir) is
   begin
      Put (" expr: ");
      Disp_Staticness (Get_Expr_Staticness (Expr));
      Put (", name: ");
      Disp_Staticness (Get_Name_Staticness (Expr));
      New_Line;
   end Disp_Name_Staticness;

   procedure Disp_Choice_Staticness (Expr: Iir) is
   begin
      Put (" choice: ");
      Disp_Staticness (Get_Choice_Staticness (Expr));
      New_Line;
   end Disp_Choice_Staticness;

   procedure Disp_Type_Resolved_Flag (Atype : Iir) is
   begin
      if Get_Resolved_Flag (Atype) then
         Put_Line ("resolved");
      else
         New_Line;
      end if;
   end Disp_Type_Resolved_Flag;

   procedure Disp_Lexical_Layout (Decl : Iir)
   is
      V : Iir_Lexical_Layout_Type;
   begin
      V := Get_Lexical_Layout (Decl);
      if (V and Iir_Lexical_Has_Mode) /= 0 then
         Put (" +mode");
      end if;
      if (V and Iir_Lexical_Has_Class) /= 0 then
         Put (" +class");
      end if;
      if (V and Iir_Lexical_Has_Type) /= 0 then
         Put (" +type");
      end if;
      New_Line;
   end Disp_Lexical_Layout;

   procedure Disp_Purity_State (State : Iir_Pure_State)
   is
   begin
      case State is
         when Pure =>
            Put (" pure");
         when Impure =>
            Put (" impure");
         when Maybe_Impure =>
            Put (" maybe_impure");
         when Unknown =>
            Put (" unknown");
      end case;
      New_Line;
   end Disp_Purity_State;

   procedure Disp_State (State : Tri_State_Type)
   is
   begin
      case State is
         when True =>
            Put (" true");
         when False =>
            Put (" false");
         when Unknown =>
            Put (" unknown");
      end case;
      New_Line;
   end Disp_State;

   procedure Disp_Depth (Depth : Iir_Int32) is
   begin
      Put (Iir_Int32'Image (Depth));
      New_Line;
   end Disp_Depth;

   procedure Disp_Tree (Tree: Iir;
                        Tab: Natural := 0;
                        Flat_Decl: Boolean := false) is
      Ntab: constant Natural := Inc_Tab (Tab);
      Kind : Iir_Kind;

      procedure Header (Str: String; Nl: Boolean := true) is
      begin
         Disp_Tab (Ntab);
         Put (Str);
         if Nl then
            New_Line;
         end if;
      end Header;

      procedure Disp_Label (Tree: Iir)is
         Label : Name_Id;
      begin
         Label := Get_Label (Tree);
         if Label /= Null_Identifier then
            Header ("label: "  & Name_Table.Image (Label));
         else
            Header ("label: -");
         end if;
      end Disp_Label;
   begin
      Disp_Tree_Flat (Tree, Tab);
      if Tree = Null_Iir then
         return;
      end if;

      if Get_Location (Tree) /= Location_Nil then
         Header ("loc: " & Errorout.Get_Location_Str (Get_Location (Tree)));
      end if;
      if False then
         Header ("parent:");
         Disp_Tree_Flat (Get_Parent (Tree), Ntab);
      end if;

      Kind := Get_Kind (Tree);
      case Kind is
         when Iir_Kind_Overload_List =>
            Header ("overload_list");
            Disp_Tree_List (Get_Overload_List (Tree), Ntab, Flat_Decl);

         when Iir_Kind_Error =>
            null;

         when Iir_Kind_Design_File =>
            Header ("design_file_filename: "
                    & Name_Table.Image (Get_Design_File_Filename (Tree)));
            Header ("design_file_directory: "
                    & Name_Table.Image (Get_Design_File_Directory (Tree)));
            Header ("analysis_time_stamp: "
                    & Files_Map.Get_Time_Stamp_String
                    (Get_Analysis_Time_Stamp (Tree)));
            Header ("file_time_stamp: "
                    & Files_Map.Get_Time_Stamp_String
                    (Get_File_Time_Stamp (Tree)));
            Header ("library:");
            Disp_Tree_Flat (Get_Parent (Tree), Ntab);
            Header ("design_unit_chain:");
            Disp_Tree_Chain (Get_First_Design_Unit (Tree), Ntab, Flat_Decl);

         when Iir_Kind_Design_Unit =>
            if Flat_Decl then
               return;
            end if;
            Header ("flags: date_state: "
                    & Date_State_Type'Image (Get_Date_State (Tree))
                    & ", elab: "
                    & Boolean'Image (Get_Elab_Flag (Tree)));
            Header ("date:" & Date_Type'Image (Get_Date (Tree)));
            Header ("parent (design file):");
            Disp_Tree_Flat (Get_Design_File (Tree), Ntab);
            Header ("dependence list:");
            Disp_Tree_List_Flat (Get_Dependence_List (Tree), Ntab);
            if Get_Date_State (Tree) /= Date_Disk then
               Header ("context items:");
               Disp_Tree_Chain (Get_Context_Items (Tree), Ntab);
            end if;
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
            Header ("library unit:");
            Disp_Tree (Get_Library_Unit (Tree), Ntab);
         when Iir_Kind_Use_Clause =>
            Header ("selected name:");
            Disp_Tree (Get_Selected_Name (Tree), Ntab, True);
            Header ("use_clause_chain:");
            Disp_Tree (Get_Use_Clause_Chain (Tree), Ntab);
         when Iir_Kind_Library_Clause =>
            Header ("library declaration:");
            Disp_Tree_Flat (Get_Library_Declaration (Tree), Ntab);

         when Iir_Kind_Library_Declaration =>
            if Flat_Decl then
               return;
            end if;
            Header ("library_directory: "
                    & Name_Table.Image (Get_Library_Directory (Tree)));
            Header ("design file list:");
            Disp_Tree_Chain (Get_Design_File_Chain (Tree), Ntab);

         when Iir_Kind_Entity_Declaration =>
            Header ("generic chain:");
            Disp_Tree_Chain (Get_Generic_Chain (Tree), Ntab);
            Header ("port chain:");
            Disp_Tree_Chain (Get_Port_Chain (Tree), Ntab);
            Header ("declaration chain:");
            Disp_Tree_Chain (Get_Declaration_Chain (Tree), Ntab);
            Header ("concurrent_statements:");
            Disp_Tree_Chain (Get_Concurrent_Statement_Chain (Tree), Ntab);
         when Iir_Kind_Package_Declaration =>
            if Flat_Decl then
               return;
            end if;
            Header ("need_body: " & Boolean'Image (Get_Need_Body (Tree)));
            Header ("declaration chain:");
            Disp_Tree_Chain (Get_Declaration_Chain (Tree), Ntab);
         when Iir_Kind_Package_Body =>
            Header ("package:");
            Disp_Tree_Flat (Get_Package (Tree), Ntab);
            Header ("declaration:");
            Disp_Tree_Chain (Get_Declaration_Chain (Tree), Ntab);
         when Iir_Kind_Architecture_Declaration =>
            if Flat_Decl then
               return;
            end if;
            Header ("entity:");
            Disp_Tree_Flat (Get_Entity (Tree), Ntab);
            Header ("declaration_chain:");
            Disp_Tree_Chain (Get_Declaration_Chain (Tree), Ntab);
            Header ("concurrent_statements:");
            Disp_Tree_Chain (Get_Concurrent_Statement_Chain (Tree), Ntab);
            Header ("default configuration:");
            Disp_Tree_Flat
              (Get_Default_Configuration_Declaration (Tree), Ntab);
         when Iir_Kind_Configuration_Declaration =>
            Header ("entity:");
            Disp_Tree_Flat (Get_Entity (Tree), Ntab);
            Header ("declaration_chain:");
            Disp_Tree_Chain (Get_Declaration_Chain (Tree), Ntab);
            Header ("block_configuration:");
            Disp_Tree (Get_Block_Configuration (Tree), Ntab, True);

         when Iir_Kind_Entity_Aspect_Entity =>
            Header ("entity:");
            Disp_Tree_Flat (Get_Entity (Tree), Ntab);
            Header ("architecture:");
            Disp_Tree_Flat (Get_Architecture (Tree), Ntab);
         when Iir_Kind_Entity_Aspect_Configuration =>
            Header ("configuration:");
            Disp_Tree (Get_Configuration (Tree), Ntab, True);
         when Iir_Kind_Entity_Aspect_Open =>
            null;

         when Iir_Kind_Block_Configuration =>
            Header ("block_specification:");
            Disp_Tree (Get_Block_Specification (Tree), Ntab, True);
            Header ("declaration_chain:");
            Disp_Tree_Chain (Get_Declaration_Chain (Tree), Ntab);
            Header ("configuration_item_chain:");
            Disp_Tree_Chain (Get_Configuration_Item_Chain (Tree), Ntab);
            Header ("prev_block_configuration:");
            Disp_Tree_Flat (Get_Prev_Block_Configuration (Tree), Ntab);
         when Iir_Kind_Attribute_Specification =>
            Header ("attribute_designator:");
            Disp_Tree (Get_Attribute_Designator (Tree), Ntab, True);
            Header ("entity_name_list:");
            Disp_Tree_List_Flat (Get_Entity_Name_List (Tree), Ntab);
            Header ("entity_class: "
                    & Tokens.Image (Get_Entity_Class (Tree)));
            Header ("expression:");
            Disp_Tree (Get_Expression (Tree), Ntab);
            Header ("attribute_value_spec_chain:");
            Disp_Tree_Chain (Get_Attribute_Value_Spec_Chain (Tree), Ntab);
         when Iir_Kind_Configuration_Specification
           | Iir_Kind_Component_Configuration =>
            Header ("instantiation_list:");
            Disp_Tree_List_Flat (Get_Instantiation_List (Tree), Ntab);
            Header ("component_name:");
            Disp_Tree (Get_Component_Name (Tree), Ntab, True);
            Header ("binding_indication:");
            Disp_Tree (Get_Binding_Indication (Tree), Ntab);
            if Kind = Iir_Kind_Component_Configuration then
               Header ("block_configuration:");
               Disp_Tree (Get_Block_Configuration (Tree), Ntab);
            end if;
         when Iir_Kind_Binding_Indication =>
            Header ("entity_aspect:");
            Disp_Tree (Get_Entity_Aspect (Tree), Ntab, True);
            Header ("generic_map_aspect_chain:");
            Disp_Tree_Chain (Get_Generic_Map_Aspect_Chain (Tree), Ntab);
            Header ("port_map_aspect_chain:");
            Disp_Tree_Chain (Get_Port_Map_Aspect_Chain (Tree), Ntab);
            Header ("default_generic_map_aspect_chain:");
            Disp_Tree_Chain
              (Get_Default_Generic_Map_Aspect_Chain (Tree), Ntab);
            Header ("default_port_map_aspect_chain:");
            Disp_Tree_Chain (Get_Default_Port_Map_Aspect_Chain (Tree), Ntab);
         when Iir_Kind_Block_Header =>
            Header ("generic chain:");
            Disp_Tree_Chain (Get_Generic_Chain (Tree), Ntab);
            Header ("generic_map_aspect_chain:");
            Disp_Tree_Chain (Get_Generic_Map_Aspect_Chain (Tree), Ntab);
            Header ("port chain:");
            Disp_Tree_Chain (Get_Port_Chain (Tree), Ntab);
            Header ("port_map_aspect_chain:");
            Disp_Tree_Chain (Get_Port_Map_Aspect_Chain (Tree), Ntab);
         when Iir_Kind_Attribute_Value =>
            Header ("staticness:", false);
            Disp_Expr_Staticness (Tree);
            Header ("attribute_specification:");
            Disp_Tree_Flat (Get_Attribute_Specification (Tree), Ntab);
            Header ("designated_entity:");
            Disp_Tree_Flat (Get_Designated_Entity (Tree), Ntab);
         when Iir_Kind_Signature =>
            Header ("return_type:");
            Disp_Tree_Flat (Get_Return_Type (Tree), Ntab);
            Header ("type_marks_list:");
            Disp_Tree_List (Get_Type_Marks_List (Tree), Ntab);
         when Iir_Kind_Disconnection_Specification =>
            Header ("signal_list:");
            Disp_Tree_List (Get_Signal_List (Tree), Ntab, True);
            Header ("type_mark:");
            Disp_Tree (Get_Type (Tree), Ntab, True);
            Header ("time expression:");
            Disp_Tree (Get_Expression (Tree), Ntab);

         when Iir_Kind_Association_Element_By_Expression =>
            Header ("whole_association_flag: ", False);
            Disp_Flag (Get_Whole_Association_Flag (Tree));
            Header ("collapse_signal_flag: ", False);
            Disp_Flag (Get_Collapse_Signal_Flag (Tree));
            Header ("formal:");
            Disp_Tree (Get_Formal (Tree), Ntab, True);
            Header ("out_conversion:");
            Disp_Tree (Get_Out_Conversion (Tree), Ntab, True);
            Header ("actual:");
            Disp_Tree (Get_Actual (Tree), Ntab, True);
            Header ("in_conversion:");
            Disp_Tree (Get_In_Conversion (Tree), Ntab, True);
         when Iir_Kind_Association_Element_By_Individual =>
            Header ("whole_association_flag: ", False);
            Disp_Flag (Get_Whole_Association_Flag (Tree));
            Header ("formal:");
            Disp_Tree (Get_Formal (Tree), Ntab, True);
            Header ("actual_type:");
            Disp_Tree (Get_Actual_Type (Tree), Ntab, True);
            Header ("individual_association_chain:");
            Disp_Tree_Chain (Get_Individual_Association_Chain (Tree), Ntab);
         when Iir_Kind_Association_Element_Open =>
            Header ("formal:");
            Disp_Tree (Get_Formal (Tree), Ntab, True);

         when Iir_Kind_Waveform_Element =>
            Header ("value:");
            Disp_Tree (Get_We_Value (Tree), Ntab, True);
            Header ("time:");
            Disp_Tree (Get_Time (Tree), Ntab);
         when Iir_Kind_Conditional_Waveform =>
            Header ("condition:");
            Disp_Tree (Get_Condition (Tree), Ntab);
            Header ("waveform_chain:");
            Disp_Tree_Chain (Get_Waveform_Chain (Tree), Ntab);

         when Iir_Kind_Choice_By_Name =>
            Header ("name:");
            Disp_Tree (Get_Name (Tree), Ntab);
            Header ("associated:");
            Disp_Tree (Get_Associated (Tree), Ntab, True);
         when Iir_Kind_Choice_By_Others =>
            Header ("associated");
            Disp_Tree (Get_Associated (Tree), Ntab, True);
         when Iir_Kind_Choice_By_None =>
            Header ("associated");
            Disp_Tree (Get_Associated (Tree), Ntab, True);
         when Iir_Kind_Choice_By_Range =>
            Header ("staticness: ", False);
            Disp_Choice_Staticness (Tree);
            Header ("range:");
            Disp_Tree (Get_Expression (Tree), Ntab);
            Header ("associated");
            Disp_Tree (Get_Associated (Tree), Ntab, True);
         when Iir_Kind_Choice_By_Expression =>
            Header ("expression:");
            Disp_Tree (Get_Expression (Tree), Ntab);
            Header ("staticness: ", False);
            Disp_Choice_Staticness (Tree);
            Header ("associated");
            Disp_Tree (Get_Associated (Tree), Ntab, True);

         when Iir_Kind_Signal_Interface_Declaration =>
            if Flat_Decl then
               return;
            end if;
            Header ("staticness: ", False);
            Disp_Name_Staticness (Tree);
            Header ("lexical layout:", False);
            Disp_Lexical_Layout (Tree);
            Header ("mode: " & Iir_Mode'Image (Get_Mode (Tree)));
            Header ("signal kind: "
                    & Iir_Signal_Kind'Image (Get_Signal_Kind (Tree)));
            Header ("has_active_flag: ", False);
            Disp_Flag (Get_Has_Active_Flag (Tree));
            Header ("type:");
            Disp_Tree (Get_Type (Tree), Ntab, True);
            Header ("default value:");
            Disp_Tree (Get_Default_Value (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Variable_Interface_Declaration =>
            if Flat_Decl then
               return;
            end if;
            Header ("staticness: ", False);
            Disp_Name_Staticness (Tree);
            Header ("lexical layout:", False);
            Disp_Lexical_Layout (Tree);
            Header ("mode: " & Iir_Mode'Image (Get_Mode (Tree)));
            Header ("type:");
            Disp_Tree (Get_Type (Tree), Ntab, True);
            Header ("default value:");
            Disp_Tree (Get_Default_Value (Tree), Ntab, True);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Constant_Interface_Declaration =>
            if Flat_Decl then
               return;
            end if;
            Header ("staticness: ", False);
            Disp_Name_Staticness (Tree);
            Header ("lexical layout:", False);
            Disp_Lexical_Layout (Tree);
            Header ("mode: " & Iir_Mode'Image (Get_Mode (Tree)));
            Header ("type:");
            Disp_Tree (Get_Type (Tree), Ntab, True);
            Header ("default value:");
            Disp_Tree (Get_Default_Value (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_File_Interface_Declaration =>
            if Flat_Decl then
               return;
            end if;
            Header ("staticness: ", False);
            Disp_Name_Staticness (Tree);
            Header ("lexical layout:", False);
            Disp_Lexical_Layout (Tree);
            Header ("mode: " & Iir_Mode'Image (Get_Mode (Tree)));
            Header ("type:");
            Disp_Tree (Get_Type (Tree), Ntab, True);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);

         when Iir_Kind_Signal_Declaration
           | Iir_Kind_Guard_Signal_Declaration =>
            if Flat_Decl then
               return;
            end if;
            Header ("kind: " & Iir_Signal_Kind'Image (Get_Signal_Kind (Tree)));
            Header ("has_active_flag: ", False);
            Disp_Flag (Get_Has_Active_Flag (Tree));
            Header ("type:");
            Disp_Tree (Get_Type (Tree), Ntab, True);
            if Kind = Iir_Kind_Signal_Declaration then
               Header ("default value:");
               Disp_Tree (Get_Default_Value (Tree), Ntab, True);
               Header ("signal_driver:");
               Disp_Tree_Flat (Get_Signal_Driver (Tree), Ntab);
            else
               Header ("guard expr:");
               Disp_Tree (Get_Guard_Expression (Tree), Ntab);
               Header ("guard sensitivity list:");
               Disp_Tree_List (Get_Guard_Sensitivity_List (Tree), Ntab);
            end if;
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration =>
            if Flat_Decl then
               return;
            end if;
            Header ("staticness:", false);
            Disp_Expr_Staticness (Tree);
            Header ("type:");
            Disp_Tree (Get_Type (Tree), Ntab, True);
            if Kind = Iir_Kind_Constant_Declaration then
               Header ("deferred flag: " & Boolean'Image
                       (Get_Deferred_Declaration_Flag (Tree)));
               Header ("deferred: ");
               Disp_Tree (Get_Deferred_Declaration (Tree), Ntab, True);
               Header ("default value:");
               Disp_Tree (Get_Default_Value (Tree), Ntab, True);
            end if;
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Variable_Declaration =>
            if Flat_Decl then
               return;
            end if;
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
            Header ("default value:");
            Disp_Tree (Get_Default_Value (Tree), Ntab, True);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_File_Declaration =>
            if Flat_Decl then
               return;
            end if;
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
            Header ("logical name:");
            Disp_Tree (Get_File_Logical_Name (Tree), Ntab);
            Header ("mode: " & Iir_Mode'Image (Get_Mode (Tree)));
            Header ("file_open_kind:");
            Disp_Tree (Get_File_Open_Kind (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration =>
            if Flat_Decl then
               return;
            end if;
            Header ("type (definition):");
            Disp_Tree (Get_Type (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Anonymous_Type_Declaration =>
            if Flat_Decl then
               return;
            end if;
            Header ("type (definition):");
            Disp_Tree (Get_Type (Tree), Ntab);
         when Iir_Kind_Component_Declaration =>
            if Flat_Decl then
               return;
            end if;
            Header ("generic chain:");
            Disp_Tree_Chain (Get_Generic_Chain (Tree), Ntab);
            Header ("port chain:");
            Disp_Tree_Chain (Get_Port_Chain (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Element_Declaration =>
            Header ("type:");
            Disp_Tree (Get_Type (Tree), Ntab, True);
         when Iir_Kind_Record_Element_Constraint =>
            Header ("type:");
            Disp_Tree (Get_Type (Tree), Ntab, True);
            Header ("element_declaration:");
            Disp_Tree (Get_Element_Declaration (Tree), Ntab);
         when Iir_Kind_Attribute_Declaration =>
            if Flat_Decl then
               return;
            end if;
            Header ("type:");
            Disp_Tree (Get_Type (Tree), Ntab, True);
         when Iir_Kind_Psl_Declaration =>
            if Flat_Decl then
               return;
            end if;
         when Iir_Kind_Psl_Expression =>
            return;
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration =>
            if Flat_Decl then
               return;
            end if;
            Header ("interface_declaration_chain:");
            Disp_Tree_Chain (Get_Interface_Declaration_Chain (Tree), Ntab);
            if Kind = Iir_Kind_Function_Declaration then
               Header ("return type:");
               Disp_Tree (Get_Return_Type (Tree), Ntab, True);
               Header ("pure_flag: ", False);
               Disp_Flag (Get_Pure_Flag (Tree));
            else
               Header ("purity_state:", False);
               Disp_Purity_State (Get_Purity_State (Tree));
            end if;
            Header ("wait_state:", False);
            Disp_State (Get_Wait_State (Tree));
            Header ("all_sensitized_state: " & Iir_All_Sensitized'Image
                      (Get_All_Sensitized_State (Tree)));
            Header ("subprogram_depth:", False);
            Disp_Depth (Get_Subprogram_Depth (Tree));
            Header ("subprogram_body:");
            Disp_Tree_Flat (Get_Subprogram_Body (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Procedure_Body
           | Iir_Kind_Function_Body =>
            Header ("specification:");
            Disp_Tree_Flat (Get_Subprogram_Specification (Tree), Ntab);
            Header ("declaration_chain:");
            Disp_Tree_Chain (Get_Declaration_Chain (Tree), Ntab);
            Header ("statements:");
            Disp_Tree_Chain (Get_Sequential_Statement_Chain (Tree), Ntab);
         when Iir_Kind_Implicit_Function_Declaration =>
            if Flat_Decl then
               return;
            end if;
            Header ("operation: "
                    & Iir_Predefined_Functions'Image
                    (Get_Implicit_Definition (Tree)));
            Header ("interface declaration chain:");
            Disp_Tree_Chain (Get_Interface_Declaration_Chain (Tree), Ntab);
            Header ("return type:");
            Disp_Tree (Get_Return_Type (Tree), Ntab, True);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Implicit_Procedure_Declaration =>
            if Flat_Decl then
               return;
            end if;
            Header ("interface declaration chain:");
            Disp_Tree_Chain (Get_Interface_Declaration_Chain (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Object_Alias_Declaration =>
            if Flat_Decl then
               return;
            end if;
            Header ("name:");
            Disp_Tree (Get_Name (Tree), Ntab);
            Header ("type:");
            Disp_Tree (Get_Type (Tree), Ntab, True);
         when Iir_Kind_Non_Object_Alias_Declaration =>
            if Flat_Decl then
               return;
            end if;
            Header ("name:");
            Disp_Tree (Get_Name (Tree), Ntab);
            Header ("signature:");
            Disp_Tree (Get_Signature (Tree), Ntab, True);

         when Iir_Kind_Group_Template_Declaration =>
            Header ("entity_class_entry:");
            Disp_Tree_Chain (Get_Entity_Class_Entry_Chain (Tree), Ntab);
         when Iir_Kind_Group_Declaration =>
            Header ("group_constituent_list:");
            Disp_Tree_List_Flat (Get_Group_Constituent_List (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);

         when Iir_Kind_Enumeration_Type_Definition =>
            if Flat_Decl then
               return;
            end if;
            Header ("staticness: ", False);
            Disp_Type_Staticness (Tree);
            Header ("type declarator:");
            Disp_Tree_Flat (Get_Type_Declarator (Tree), Ntab);
            Header ("literals:");
            Disp_Tree_List (Get_Enumeration_Literal_List (Tree), Ntab);
         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Type_Definition =>
            if Flat_Decl and then not Is_Anonymous_Type_Definition (Tree)
            then
               return;
            end if;
            Header ("staticness: ", False);
            Disp_Type_Staticness (Tree);
            Header ("type_declarator:");
            Disp_Tree_Flat (Get_Type_Declarator (Tree), Ntab);
         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Subtype_Definition =>
            if Flat_Decl
              and then Kind /= Iir_Kind_Subtype_Definition
              and then Get_Type_Declarator (Tree) /= Null_Iir
            then
               return;
            end if;
            if Kind /= Iir_Kind_Subtype_Definition then
               Header ("staticness: ", False);
               Disp_Type_Staticness (Tree);
               Header ("resolved flag: ", False);
               Disp_Type_Resolved_Flag (Tree);
               Header ("signal_type_flag: ", False);
               Disp_Flag (Get_Signal_Type_Flag (Tree));
               Header ("has_signal_flag: ", False);
               Disp_Flag (Get_Has_Signal_Flag (Tree));
               Header ("type declarator:");
               Disp_Tree (Get_Type_Declarator (Tree), Ntab, True);
               Header ("base type:");
               Disp_Tree (Get_Base_Type (Tree), Ntab, True);
            end if;
            Header ("type mark:");
            Disp_Tree (Get_Type_Mark (Tree), Ntab, True);
            Header ("resolution function:");
            Disp_Tree_Flat (Get_Resolution_Function (Tree), Ntab);
            Header ("range constraint:");
            Disp_Tree (Get_Range_Constraint (Tree), Ntab);
         when Iir_Kind_Range_Expression =>
            Header ("staticness:", false);
            Disp_Expr_Staticness (Tree);
            Header ("left limit:");
            Disp_Tree (Get_Left_Limit (Tree), Ntab, True);
            Header ("right limit:");
            Disp_Tree (Get_Right_Limit (Tree), Ntab, True);
            Header ("direction: "
                    & Iir_Direction'Image (Get_Direction (Tree)));
            Header ("type:");
            Disp_Tree (Get_Type (Tree), Ntab, True);
         when Iir_Kind_Array_Subtype_Definition =>
            if Flat_Decl and then Get_Type_Declarator (Tree) /= Null_Iir then
               return;
            end if;
            Header ("staticness:", false);
            Disp_Type_Staticness (Tree);
            Header ("resolved flag: ", False);
            Disp_Type_Resolved_Flag (Tree);
            Header ("signal_type_flag: ", False);
            Disp_Flag (Get_Signal_Type_Flag (Tree));
            Header ("has_signal_flag: ", False);
            Disp_Flag (Get_Has_Signal_Flag (Tree));
            Header ("type declarator:");
            Disp_Tree_Flat (Get_Type_Declarator (Tree), Ntab);
            Header ("base type:");
            declare
               Base : constant Iir := Get_Base_Type (Tree);
               Fl : Boolean;
            begin
               if Base /= Null_Iir
                 and then Get_Kind (Base) = Iir_Kind_Array_Type_Definition
               then
                  Fl := Get_Type_Declarator (Base)
                    /= Get_Type_Declarator (Tree);
               else
                  Fl := False;
               end if;
               Disp_Tree (Base, Ntab, Fl);
            end;
            Header ("type mark:");
            Disp_Tree (Get_Type_Mark (Tree), Ntab, True);
            Header ("index_subtype_list:");
            Disp_Tree_List (Get_Index_Subtype_List (Tree), Ntab, True);
            Header ("element_subtype:");
            Disp_Tree (Get_Element_Subtype (Tree), Ntab, True);
            Header ("resolution function:");
            Disp_Tree_Flat (Get_Resolution_Function (Tree), Ntab);
            Header ("index_constraint: ", False);
            Disp_Flag (Get_Index_Constraint_Flag (Tree));
            Header ("constraint_state: "
                      & Iir_Constraint'Image (Get_Constraint_State (Tree)));
         when Iir_Kind_Array_Type_Definition  =>
            if Flat_Decl and then Get_Type_Declarator (Tree) /= Null_Iir then
               return;
            end if;
            Header ("staticness: ", False);
            Disp_Type_Staticness (Tree);
            Header ("resolved flag: ", False);
            Disp_Type_Resolved_Flag (Tree);
            Header ("signal_type_flag: ", False);
            Disp_Flag (Get_Signal_Type_Flag (Tree));
            Header ("has_signal_flag: ", False);
            Disp_Flag (Get_Has_Signal_Flag (Tree));
            Header ("index_subtype_list:");
            Disp_Tree_List (Get_Index_Subtype_List (Tree), Ntab, True);
            Header ("element_subtype:");
            Disp_Tree (Get_Element_Subtype (Tree), Ntab, True);
         when Iir_Kind_Record_Type_Definition =>
            if Flat_Decl and then Get_Type_Declarator (Tree) /= Null_Iir then
               return;
            end if;
            Header ("staticness: ", False);
            Disp_Type_Staticness (Tree);
            Header ("resolved flag: ", False);
            Disp_Type_Resolved_Flag (Tree);
            Header ("signal_type_flag: ", False);
            Disp_Flag (Get_Signal_Type_Flag (Tree));
            Header ("has_signal_flag: ", False);
            Disp_Flag (Get_Has_Signal_Flag (Tree));
            Header ("constraint_state: "
                      & Iir_Constraint'Image (Get_Constraint_State (Tree)));
            Header ("elements:");
            Disp_Tree_List (Get_Elements_Declaration_List (Tree), Ntab, True);
         when Iir_Kind_Record_Subtype_Definition =>
            if Flat_Decl and then not Is_Anonymous_Type_Definition (Tree) then
               return;
            end if;
            Header ("type declarator:");
            Disp_Tree_Flat (Get_Type_Declarator (Tree), Ntab);
            Header ("resolved flag: ", False);
            Disp_Type_Resolved_Flag (Tree);
            Header ("signal_type_flag: ", False);
            Disp_Flag (Get_Signal_Type_Flag (Tree));
            Header ("base type:");
            Disp_Tree (Get_Base_Type (Tree), Ntab, True);
            Header ("type mark:");
            Disp_Tree (Get_Type_Mark (Tree), Ntab, True);
            Header ("resolution function:");
            Disp_Tree_Flat (Get_Resolution_Function (Tree), Ntab);
            Header ("constraint_state: "
                      & Iir_Constraint'Image (Get_Constraint_State (Tree)));
            Header ("elements:");
            Disp_Tree_List (Get_Elements_Declaration_List (Tree), Ntab, True);
         when Iir_Kind_Physical_Type_Definition =>
            if Flat_Decl and then Get_Type_Declarator (Tree) /= Null_Iir then
               return;
            end if;
            Header ("staticness: ", False);
            Disp_Type_Staticness (Tree);
            Header ("resolved flag: ", False);
            Disp_Type_Resolved_Flag (Tree);
            Header ("declarator:");
            Disp_Tree_Flat (Get_Type_Declarator (Tree), Ntab);
            Header ("unit chain:");
            Disp_Tree_Chain (Get_Unit_Chain (Tree), Ntab);
         when Iir_Kind_Unit_Declaration =>
            if Flat_Decl then
               return;
            end if;
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
            Header ("physical_literal:");
            Disp_Tree (Get_Physical_Literal (Tree), Ntab, True);
            Header ("physical_Unit_Value:");
            Disp_Tree (Get_Physical_Unit_Value (Tree), Ntab, True);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);

         when Iir_Kind_Access_Type_Definition =>
            if Flat_Decl then
               return;
            end if;
            Header ("staticness: ", False);
            Disp_Type_Staticness (Tree);
            Header ("resolved flag: ", False);
            Disp_Type_Resolved_Flag (Tree);
            Header ("signal_type_flag: ", False);
            Disp_Flag (Get_Signal_Type_Flag (Tree));
            Header ("declarator:");
            Disp_Tree_Flat (Get_Type_Declarator (Tree), Ntab);
            Header ("designated type:");
            Disp_Tree_Flat (Get_Designated_Type (Tree), Ntab);
         when Iir_Kind_Access_Subtype_Definition =>
            Header ("staticness: ", False);
            Disp_Type_Staticness (Tree);
            Header ("resolved flag: ", False);
            Disp_Type_Resolved_Flag (Tree);
            Header ("declarator:");
            Disp_Tree_Flat (Get_Type_Declarator (Tree), Ntab);
            Header ("base type:");
            Disp_Tree (Get_Base_Type (Tree), Ntab, True);
            Header ("type mark:");
            Disp_Tree (Get_Type_Mark (Tree), Ntab, True);
            Header ("designated type:");
            Disp_Tree_Flat (Get_Designated_Type (Tree), Ntab);
            Header ("resolution function:");
            Disp_Tree_Flat (Get_Resolution_Function (Tree), Ntab);

         when Iir_Kind_Incomplete_Type_Definition =>
            Header ("staticness: ", False);
            Disp_Type_Staticness (Tree);
            Header ("declarator:");
            Disp_Tree_Flat (Get_Type_Declarator (Tree), Ntab);
            Header ("base type:");
            Disp_Tree (Get_Base_Type (Tree), Ntab, True);

         when Iir_Kind_File_Type_Definition =>
            Header ("staticness: ", False);
            Disp_Type_Staticness (Tree);
            Header ("declarator:");
            Disp_Tree_Flat (Get_Type_Declarator (Tree), Ntab);
            Header ("type mark:");
            Disp_Tree_Flat (Get_Type_Mark (Tree), Ntab);
         when Iir_Kind_Protected_Type_Declaration =>
            Header ("staticness: ", False);
            Disp_Type_Staticness (Tree);
            Header ("declarator:");
            Disp_Tree_Flat (Get_Type_Declarator (Tree), Ntab);
            Header ("protected_type_body:");
            Disp_Tree_Flat (Get_Protected_Type_Body (Tree), Ntab);
            Header ("declarative_part:");
            Disp_Tree_Chain (Get_Declaration_Chain (Tree), Ntab);
         when Iir_Kind_Protected_Type_Body =>
            Header ("protected_type_declaration:");
            Disp_Tree_Flat (Get_Protected_Type_Declaration (Tree), Ntab);
            Header ("declarative_part:");
            Disp_Tree_Chain (Get_Declaration_Chain (Tree), Ntab);

         when Iir_Kind_Block_Statement =>
            if Flat_Decl then
               return;
            end if;
            Disp_Label (Tree);
            Header ("guard decl:");
            Disp_Tree (Get_Guard_Decl (Tree), Ntab);
            Header ("block header:");
            Disp_Tree (Get_Block_Header (Tree), Ntab);
            Header ("declaration_chain:");
            Disp_Tree_Chain (Get_Declaration_Chain (Tree), Ntab);
            Header ("concurrent statements:");
            Disp_Tree_Chain (Get_Concurrent_Statement_Chain (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Generate_Statement =>
            if Flat_Decl then
               return;
            end if;
            Disp_Label (Tree);
            Header ("generation_scheme:");
            Disp_Tree (Get_Generation_Scheme (Tree), Ntab);
            Header ("declaration_chain:");
            Disp_Tree_Chain (Get_Declaration_Chain (Tree), Ntab);
            Header ("concurrent statements:");
            Disp_Tree_Chain (Get_Concurrent_Statement_Chain (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);

         when Iir_Kind_Component_Instantiation_Statement =>
            Disp_Label (Tree);
            Header ("instantiated unit:");
            Disp_Tree (Get_Instantiated_Unit (Tree), Ntab, True);
            Header ("generic map aspect chain:");
            Disp_Tree_Chain (Get_Generic_Map_Aspect_Chain (Tree), Ntab);
            Header ("port map aspect chain:");
            Disp_Tree_Chain (Get_Port_Map_Aspect_Chain (Tree), Ntab);
            Header ("component_configuration:");
            Disp_Tree (Get_Component_Configuration (Tree), Ntab);
            Header ("default binding indication:");
            Disp_Tree (Get_Default_Binding_Indication (Tree), Ntab, True);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
            Header ("guarded_target_flag: "
                    & Tri_State_Type'Image (Get_Guarded_Target_State (Tree)));
            Header ("target:");
            Disp_Tree (Get_Target (Tree), Ntab, True);
            if Get_Guard (Tree) = Tree then
               Header ("guard: guarded");
            else
               Header ("guard:");
               Disp_Tree_Flat (Get_Guard (Tree), Ntab);
            end if;
            Header ("conditional waveform chain:");
            Disp_Tree_Chain (Get_Conditional_Waveform_Chain (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Concurrent_Selected_Signal_Assignment =>
            Header ("guarded_target_flag: "
                    & Tri_State_Type'Image (Get_Guarded_Target_State (Tree)));
            Header ("target:");
            Disp_Tree (Get_Target (Tree), Ntab, True);
            if Get_Guard (Tree) = Tree then
               Header ("guard: guarded");
            else
               Header ("guard:");
               Disp_Tree_Flat (Get_Guard (Tree), Ntab);
            end if;
            Header ("choices:");
            Disp_Tree_Chain (Get_Selected_Waveform_Chain (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Concurrent_Assertion_Statement =>
            Header ("condition:");
            Disp_Tree (Get_Assertion_Condition (Tree), Ntab);
            Header ("report expression:");
            Disp_Tree (Get_Report_Expression (Tree), Ntab);
            Header ("severity expression:");
            Disp_Tree (Get_Severity_Expression (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Psl_Assert_Statement =>
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
            PSL.Dump_Tree.Dump_Tree (Get_Psl_Property (Tree), True);
         when Iir_Kind_Psl_Default_Clock =>
            null;

         when Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement =>
            Disp_Label (Tree);
            Header ("passive: " & Boolean'Image (Get_Passive_Flag (Tree)));
            if Kind = Iir_Kind_Sensitized_Process_Statement then
               Header ("sensivity list:");
               Disp_Tree_List (Get_Sensitivity_List (Tree), Ntab, True);
            end if;
            Header ("declaration_chain:");
            Disp_Tree_Chain (Get_Declaration_Chain (Tree), Ntab);
            Header ("process statements:");
            Disp_Tree_Chain (Get_Sequential_Statement_Chain (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_If_Statement =>
            Header ("condition:");
            Disp_Tree (Get_Condition (Tree), Ntab, True);
            Header ("then sequence:");
            Disp_Tree_Chain (Get_Sequential_Statement_Chain (Tree), Ntab);
            Header ("elsif:");
            Disp_Tree (Get_Else_Clause (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Elsif =>
            Header ("condition:");
            Disp_Tree (Get_Condition (Tree), Ntab);
            Header ("then sequence:");
            Disp_Tree_Chain (Get_Sequential_Statement_Chain (Tree), Ntab);
            Header ("elsif:");
            Disp_Tree (Get_Else_Clause (Tree), Tab);
         when Iir_Kind_For_Loop_Statement =>
            Header ("iterator:");
            Disp_Tree (Get_Iterator_Scheme (Tree), Ntab);
            Header ("statements:");
            Disp_Tree_Chain (Get_Sequential_Statement_Chain (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_While_Loop_Statement =>
            Header ("condition:");
            Disp_Tree (Get_Condition (Tree), Ntab);
            Header ("statements:");
            Disp_Tree_Chain (Get_Sequential_Statement_Chain (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Case_Statement =>
            Header ("expression:");
            Disp_Tree (Get_Expression (Tree), Ntab, True);
            Header ("choices chain:");
            Disp_Tree_Chain
              (Get_Case_Statement_Alternative_Chain (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Signal_Assignment_Statement =>
            Header ("guarded_target_flag: "
                    & Tri_State_Type'Image (Get_Guarded_Target_State (Tree)));
            Header ("target:");
            Disp_Tree (Get_Target (Tree), Ntab, True);
            Header ("waveform_chain:");
            Disp_Tree_Chain (Get_Waveform_Chain (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Variable_Assignment_Statement =>
            Header ("target:");
            Disp_Tree (Get_Target (Tree), Ntab, True);
            Header ("expression:");
            Disp_Tree (Get_Expression (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Assertion_Statement =>
            Header ("condition:");
            Disp_Tree (Get_Assertion_Condition (Tree), Ntab);
            Header ("report expression:");
            Disp_Tree (Get_Report_Expression (Tree), Ntab);
            Header ("severity expression:");
            Disp_Tree (Get_Severity_Expression (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Report_Statement =>
            Header ("report expression:");
            Disp_Tree (Get_Report_Expression (Tree), Ntab);
            Header ("severity expression:");
            Disp_Tree (Get_Severity_Expression (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Return_Statement =>
            Header ("expression:");
            Disp_Tree (Get_Expression (Tree), Ntab, True);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Wait_Statement =>
            Header ("sensitivity list:");
            Disp_Tree_List (Get_Sensitivity_List (Tree), Ntab, True);
            Header ("condition:");
            Disp_Tree (Get_Condition_Clause (Tree), Ntab);
            Header ("timeout:");
            Disp_Tree (Get_Timeout_Clause (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Procedure_Call_Statement
           | Iir_Kind_Concurrent_Procedure_Call_Statement =>
            Disp_Label (Tree);
            Header ("procedure_call:");
            Disp_Tree (Get_Procedure_Call (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Procedure_Call =>
            Header ("implementation:");
            Disp_Tree (Get_Implementation (Tree), Ntab, True);
            Header ("method_object:");
            Disp_Tree (Get_Method_Object (Tree), Ntab);
            Header ("parameters:");
            Disp_Tree_Chain (Get_Parameter_Association_Chain (Tree), Ntab);
         when Iir_Kind_Exit_Statement
           | Iir_Kind_Next_Statement =>
            Header ("loop:");
            Disp_Tree_Flat (Get_Loop (Tree), Ntab);
            Header ("condition:");
            Disp_Tree (Get_Condition (Tree), Ntab);
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
         when Iir_Kind_Null_Statement =>
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);

         when Iir_Kinds_Dyadic_Operator =>
            Header ("staticness:", false);
            Disp_Expr_Staticness (Tree);
            Header ("implementation:");
            Disp_Tree (Get_Implementation (Tree), Ntab, True);
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
            Header ("left:");
            Disp_Tree (Get_Left (Tree), Ntab, True);
            Header ("right:");
            Disp_Tree (Get_Right (Tree), Ntab, True);

         when Iir_Kinds_Monadic_Operator =>
            Header ("staticness:", false);
            Disp_Expr_Staticness (Tree);
            Header ("implementation:");
            Disp_Tree (Get_Implementation (Tree), Ntab, True);
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
            Header ("operand:");
            Disp_Tree (Get_Operand (Tree), Ntab, True);

         when Iir_Kind_Function_Call =>
            Header ("staticness:", false);
            Disp_Expr_Staticness (Tree);
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
            Header ("implementation:");
            Disp_Tree_Flat (Get_Implementation (Tree), Ntab);
            Header ("method_object:");
            Disp_Tree (Get_Method_Object (Tree), Ntab);
            Header ("parameters:");
            Disp_Tree_Chain (Get_Parameter_Association_Chain (Tree), Ntab);
         when Iir_Kind_Qualified_Expression =>
            Header ("staticness:", false);
            Disp_Expr_Staticness (Tree);
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
            Header ("type mark:");
            Disp_Tree (Get_Type_Mark (Tree), Ntab, True);
            Header ("expression:");
            Disp_Tree (Get_Expression (Tree), Ntab, True);
         when Iir_Kind_Type_Conversion =>
            Header ("staticness:", false);
            Disp_Expr_Staticness (Tree);
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
            Header ("expression:");
            Disp_Tree (Get_Expression (Tree), Ntab, True);
         when Iir_Kind_Allocator_By_Expression =>
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
            Header ("expression:");
            Disp_Tree (Get_Expression (Tree), Ntab, True);
         when Iir_Kind_Allocator_By_Subtype =>
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
            Header ("subtype indication:");
            Disp_Tree (Get_Expression (Tree), Ntab, True);
         when Iir_Kind_Selected_Element =>
            Header ("prefix:");
            Disp_Tree (Get_Prefix (Tree), Ntab, True);
            Header ("selected element:");
            Disp_Tree (Get_Selected_Element (Tree), Ntab, True);
         when Iir_Kind_Implicit_Dereference
           | Iir_Kind_Dereference =>
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
            Header ("prefix:");
            Disp_Tree (Get_Prefix (Tree), Ntab, True);

         when Iir_Kind_Aggregate =>
            Header ("staticness: value: ", false);
            Disp_Staticness (Get_Value_Staticness (Tree));
            Disp_Expr_Staticness (Tree);
            Header ("type:");
            Disp_Tree (Get_Type (Tree), Ntab, True);
            Header ("aggregate_info:");
            Disp_Tree (Get_Aggregate_Info (Tree), Ntab);
            Header ("associations:");
            Disp_Tree_Chain (Get_Association_Choices_Chain (Tree), Ntab);
         when Iir_Kind_Aggregate_Info =>
            Header ("aggr_others_flag: ", False);
            Disp_Flag (Get_Aggr_Others_Flag (Tree));
            Header ("aggr_named_flag: ", False);
            Disp_Flag (Get_Aggr_Named_Flag (Tree));
            Header ("aggr_dynamic_flag: ", False);
            Disp_Flag (Get_Aggr_Dynamic_Flag (Tree));
            Header ("aggr_low_limit:");
            Disp_Tree (Get_Aggr_Low_Limit (Tree), Ntab, False);
            Header ("aggr_high_limit:");
            Disp_Tree (Get_Aggr_High_Limit (Tree), Ntab, False);
            Header ("aggr_max_length:" &
                    Iir_Int32'Image (Get_Aggr_Max_Length (Tree)));
            Header ("sub_aggregate_info:");
            Disp_Tree (Get_Sub_Aggregate_Info (Tree), Ntab);
         when Iir_Kind_Operator_Symbol =>
            null;
         when Iir_Kind_Simple_Name =>
            Header ("staticness:", false);
            Disp_Expr_Staticness (Tree);
            Header ("type:");
            Disp_Tree (Get_Type (Tree), Ntab, True);
         when Iir_Kind_Indexed_Name =>
            Header ("staticness:", false);
            Disp_Name_Staticness (Tree);
            Header ("prefix:");
            Disp_Tree (Get_Prefix (Tree), Ntab, True);
            Header ("index:");
            Disp_Tree_List (Get_Index_List (Tree), Ntab, True);
            Header ("type:");
            Disp_Tree (Get_Type (Tree), Ntab, True);
         when Iir_Kind_Slice_Name =>
            Header ("staticness:", false);
            Disp_Name_Staticness (Tree);
            Header ("prefix:");
            Disp_Tree (Get_Prefix (Tree), Ntab, True);
            Header ("suffix:");
            Disp_Tree (Get_Suffix (Tree), Ntab);
            Header ("type:");
            Disp_Tree (Get_Type (Tree), Ntab, True);
         when Iir_Kind_Parenthesis_Name =>
            Header ("prefix:");
            Disp_Tree (Get_Prefix (Tree), Ntab, Flat_Decl);
            Header ("association chain:");
            Disp_Tree_Chain (Get_Association_Chain (Tree), Ntab);
         when Iir_Kind_Selected_By_All_Name =>
            Header ("prefix:");
            Disp_Tree (Get_Prefix (Tree), Ntab, True);
            Header ("type:");
            Disp_Tree (Get_Type (Tree), Ntab, True);
         when Iir_Kind_Selected_Name =>
            Header ("prefix:");
            Disp_Tree (Get_Prefix (Tree), Ntab, True);
            Header ("identifier: ", False);
            Disp_Ident (Get_Suffix_Identifier (Tree));

         when Iir_Kind_Attribute_Name =>
            Header ("prefix:");
            Disp_Tree (Get_Prefix (Tree), Ntab, True);
            Header ("signature:");
            Disp_Tree (Get_Signature (Tree), Ntab);

         when Iir_Kind_Base_Attribute =>
            Header ("prefix:");
            Disp_Tree_Flat (Get_Prefix (Tree), Ntab);
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
         when Iir_Kind_Left_Type_Attribute
           | Iir_Kind_Right_Type_Attribute
           | Iir_Kind_High_Type_Attribute
           | Iir_Kind_Low_Type_Attribute
           | Iir_Kind_Ascending_Type_Attribute =>
            Header ("staticness:", false);
            Disp_Expr_Staticness (Tree);
            Header ("prefix:");
            Disp_Tree_Flat (Get_Prefix (Tree), Ntab);
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
         when Iir_Kind_Image_Attribute
           | Iir_Kind_Value_Attribute =>
            Header ("prefix:");
            Disp_Tree_Flat (Get_Prefix (Tree), Ntab);
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
            Header ("parameter:");
            Disp_Tree (Get_Parameter (Tree), Ntab);
         when Iir_Kind_Pos_Attribute
            | Iir_Kind_Val_Attribute
            | Iir_Kind_Succ_Attribute
            | Iir_Kind_Pred_Attribute
            | Iir_Kind_Leftof_Attribute
            | Iir_Kind_Rightof_Attribute =>
            Header ("staticness:", false);
            Disp_Expr_Staticness (Tree);
            Header ("prefix:");
            Disp_Tree_Flat (Get_Prefix (Tree), Ntab);
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
            Header ("parameter:");
            Disp_Tree (Get_Parameter (Tree), Ntab);
         when Iir_Kind_Left_Array_Attribute
           | Iir_Kind_Right_Array_Attribute
           | Iir_Kind_High_Array_Attribute
           | Iir_Kind_Low_Array_Attribute
           | Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute
           | Iir_Kind_Length_Array_Attribute
           | Iir_Kind_Ascending_Array_Attribute =>
            Header ("staticness:", false);
            Disp_Expr_Staticness (Tree);
            Header ("prefix:");
            Disp_Tree_Flat (Get_Prefix (Tree), Ntab);
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
            Header ("parameter:");
            Disp_Tree (Get_Parameter (Tree), Ntab);
         when Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute =>
            Header ("prefix:");
            Disp_Tree_Flat (Get_Prefix (Tree), Ntab);
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
            if Kind /= Iir_Kind_Transaction_Attribute then
               Header ("parameter:");
               Disp_Tree (Get_Parameter (Tree), Ntab);
            end if;
            Header ("has_active_flag: ", False);
            Disp_Flag (Get_Has_Active_Flag (Tree));
         when Iir_Kind_Event_Attribute
           | Iir_Kind_Active_Attribute
           | Iir_Kind_Last_Event_Attribute
           | Iir_Kind_Last_Active_Attribute
           | Iir_Kind_Last_Value_Attribute
           | Iir_Kind_Driving_Attribute
           | Iir_Kind_Driving_Value_Attribute =>
            Header ("prefix:");
            Disp_Tree_Flat (Get_Prefix (Tree), Ntab);
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
         when Iir_Kind_Behavior_Attribute
           | Iir_Kind_Structure_Attribute =>
            Header ("prefix:");
            Disp_Tree_Flat (Get_Prefix (Tree), Ntab);
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
         when Iir_Kind_Simple_Name_Attribute
           | Iir_Kind_Instance_Name_Attribute
           | Iir_Kind_Path_Name_Attribute =>
            Header ("prefix:");
            Disp_Tree_Flat (Get_Prefix (Tree), Ntab);
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);

         when Iir_Kind_Enumeration_Literal =>
            if Flat_Decl and then Get_Literal_Origin (Tree) = Null_Iir then
               return;
            end if;
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
            Header ("value:" & Iir_Int32'Image (Get_Enum_Pos (Tree)));
            Header ("attribute_value_chain:");
            Disp_Tree_Flat_Chain (Get_Attribute_Value_Chain (Tree), Ntab);
            Header ("origin:");
            Disp_Tree (Get_Literal_Origin (Tree), Ntab, True);
         when Iir_Kind_Integer_Literal =>
            Header ("staticness:", false);
            Disp_Expr_Staticness (Tree);
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
            Header ("origin:");
            Disp_Tree (Get_Literal_Origin (Tree), Ntab, True);
         when Iir_Kind_Floating_Point_Literal =>
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
            Header ("origin:");
            Disp_Tree (Get_Literal_Origin (Tree), Ntab, True);
         when Iir_Kind_String_Literal =>
            Header ("value: """ & Iirs_Utils.Image_String_Lit (Tree) & """");
            Header ("type:");
            Disp_Tree (Get_Type (Tree), Ntab, True);
            Header ("origin:");
            Disp_Tree (Get_Literal_Origin (Tree), Ntab, True);
         when Iir_Kind_Bit_String_Literal =>
            Header ("base: " & Base_Type'Image (Get_Bit_String_Base (Tree)));
            Header ("value: """ & Iirs_Utils.Image_String_Lit (Tree) & """");
            Header ("len:" & Int32'Image (Get_String_Length (Tree)));
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
         when Iir_Kind_Character_Literal =>
            Header ("value: '" &
                    Name_Table.Get_Character (Get_Identifier (Tree)) &
                    ''');
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
         when Iir_Kind_Physical_Int_Literal =>
            Header ("staticness:", False);
            Disp_Expr_Staticness (Tree);
            Header ("value: " & Iir_Int64'Image (Get_Value (Tree)));
            Header ("unit_name: ");
            Disp_Tree_Flat (Get_Unit_Name (Tree), Ntab);
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
            Header ("origin:");
            Disp_Tree (Get_Literal_Origin (Tree), Ntab);
         when Iir_Kind_Physical_Fp_Literal =>
            Header ("staticness:", False);
            Disp_Expr_Staticness (Tree);
            Header ("fp_value: " & Iir_Fp64'Image (Get_Fp_Value (Tree)));
            Header ("unit_name: ");
            Disp_Tree_Flat (Get_Unit_Name (Tree), Ntab);
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
            Header ("origin:");
            Disp_Tree (Get_Literal_Origin (Tree), Ntab);
         when Iir_Kind_Null_Literal =>
            Header ("type:");
            Disp_Tree_Flat (Get_Type (Tree), Ntab);
         when Iir_Kind_Simple_Aggregate =>
            Header ("simple_aggregate_list:");
            Disp_Tree_List (Get_Simple_Aggregate_List (Tree), Ntab, True);
            Header ("type:");
            Disp_Tree (Get_Type (Tree), Ntab, True);
            Header ("origin:");
            Disp_Tree (Get_Literal_Origin (Tree), Ntab, True);

         when Iir_Kind_Proxy =>
            Header ("proxy:");
            Disp_Tree_Flat (Get_Proxy (Tree), Ntab);
         when Iir_Kind_Entity_Class =>
            null;
      end case;
   end Disp_Tree;

   procedure Disp_Tree_For_Psl (N : Int32) is
   begin
      Disp_Tree_Flat (Iir (N), 1);
   end Disp_Tree_For_Psl;
end Disp_Tree;
