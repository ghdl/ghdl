--  VHDL regeneration from internal nodes.
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

--  Re-print a tree as VHDL sources.  Except for comments and parenthesis, the
--  sequence of tokens displayed is the same as the sequence of tokens in the
--  input file.  If parenthesis are kept by the parser, the only differences
--  are comments and layout.
with GNAT.OS_Lib;
with Std_Package;
with Flags; use Flags;
with Errorout; use Errorout;
with Iirs_Utils; use Iirs_Utils;
with Name_Table;
with Str_Table;
with Std_Names;
with Tokens;
with PSL.Nodes;
with PSL.Prints;
with PSL.NFAs;

package body Disp_Vhdl is

   subtype Count is Positive;

   Col : Count := 1;

   IO_Error : exception;

   --  Disp the name of DECL.
   procedure Disp_Name_Of (Decl: Iir);

   --  Indentation for nested declarations and statements.
   Indentation: constant Count := 2;

   --  Line length (used to try to have a nice display).
   Line_Length : constant Count := 80;

   --  If True, display extra parenthesis to make priority of operators
   --  explicit.
   Flag_Parenthesis : constant Boolean := False;

   -- If set, disp after a string literal the type enclosed into brackets.
   Flag_Disp_String_Literal_Type: constant Boolean := False;

   -- If set, disp position number of associations
   --Disp_Position_Number: constant Boolean := False;

--    procedure Disp_Tab (Tab: Natural) is
--       Blanks : String (1 .. Tab) := (others => ' ');
--    begin
--       Put (Blanks);
--    end Disp_Tab;

   procedure Disp_Type (A_Type: Iir);
   procedure Disp_Nature (Nature : Iir);
   procedure Disp_Range (Rng : Iir);

   procedure Disp_Concurrent_Statement (Stmt: Iir);
   procedure Disp_Concurrent_Statement_Chain (Parent: Iir; Indent : Count);
   procedure Disp_Declaration_Chain (Parent : Iir; Indent: Count);
   procedure Disp_Process_Statement (Process: Iir);
   procedure Disp_Sequential_Statements (First : Iir);
   procedure Disp_Choice (Choice: in out Iir);
   procedure Disp_Association_Chain (Chain : Iir);
   procedure Disp_Block_Configuration
     (Block: Iir_Block_Configuration; Indent: Count);
   procedure Disp_Subprogram_Declaration (Subprg: Iir);
   procedure Disp_Binding_Indication (Bind : Iir; Indent : Count);
   procedure Disp_Subtype_Indication (Def : Iir; Full_Decl : Boolean := False);
   procedure Disp_Parametered_Attribute (Name : String; Expr : Iir);
   procedure Disp_String_Literal (Str : Iir; El_Type : Iir);
   procedure Disp_Package_Declaration (Decl: Iir_Package_Declaration);
   procedure Disp_Package_Instantiation_Declaration (Decl: Iir);
   procedure Disp_Package_Body (Decl: Iir);

   procedure Put (Str : String)
   is
      use GNAT.OS_Lib;
      Len : constant Natural := Str'Length;
   begin
      if Write (Standout, Str'Address, Len) /= Len then
         raise IO_Error;
      end if;
      Col := Col + Len;
   end Put;

   procedure Put (C : Character) is
   begin
      Put ((1 => C));
   end Put;

   procedure New_Line is
   begin
      Put (ASCII.LF);
      Col := 1;
   end New_Line;

   procedure Put_Line (Str : String) is
   begin
      Put (Str);
      New_Line;
   end Put_Line;

   procedure Set_Col (P : Count) is
   begin
      if Col = P then
         return;
      end if;
      if Col >= P then
         New_Line;
      end if;
      Put ((Col .. P - 1 => ' '));
   end Set_Col;

   procedure Disp_Ident (Id: Name_Id) is
   begin
      Put (Name_Table.Image (Id));
   end Disp_Ident;

   procedure Disp_Identifier (Node : Iir)
   is
      Ident : Name_Id;
   begin
      Ident := Get_Identifier (Node);
      if Ident /= Null_Identifier then
         Disp_Ident (Ident);
      else
         Put ("<anonymous>");
      end if;
   end Disp_Identifier;

   procedure Disp_Character_Literal (Lit: Iir_Character_Literal) is
   begin
      Put (''' & Name_Table.Get_Character (Get_Identifier (Lit)) & ''');
   end Disp_Character_Literal;

   procedure Disp_Function_Name (Func: Iir)
   is
      use Name_Table;
      use Std_Names;
      Id: Name_Id;
   begin
      Id := Get_Identifier (Func);
      case Id is
         when Name_Id_Operators
           | Name_Word_Operators
           | Name_Logical_Operators
           | Name_Xnor
           | Name_Shift_Operators =>
            Put ("""");
            Put (Image (Id));
            Put ("""");
         when others =>
            Disp_Ident (Id);
      end case;
   end Disp_Function_Name;

   --  Disp the name of DECL.
   procedure Disp_Name_Of (Decl: Iir) is
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Component_Declaration
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Architecture_Body
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Context_Declaration
           | Iir_Kinds_Interface_Object_Declaration
           | Iir_Kind_Interface_Type_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Type_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Element_Declaration
           | Iir_Kind_Record_Element_Constraint
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Library_Declaration
           | Iir_Kind_Unit_Declaration
           | Iir_Kind_Nature_Declaration
           | Iir_Kind_Terminal_Declaration
           | Iir_Kinds_Quantity_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Character_Literal
           | Iir_Kinds_Process_Statement =>
            Disp_Identifier (Decl);
         when Iir_Kind_Anonymous_Type_Declaration =>
            Put ('<');
            Disp_Ident (Get_Identifier (Decl));
            Put ('>');
         when Iir_Kind_Function_Declaration =>
            Disp_Function_Name (Decl);
         when Iir_Kind_Procedure_Declaration =>
            Disp_Identifier (Decl);
         when Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Protected_Type_Declaration =>
            --  Used for 'end' DECL_NAME.
            Disp_Identifier (Get_Type_Declarator (Decl));
         when Iir_Kind_Component_Instantiation_Statement =>
            Disp_Ident (Get_Label (Decl));
         when Iir_Kind_Design_Unit =>
            Disp_Name_Of (Get_Library_Unit (Decl));
         when Iir_Kind_Enumeration_Literal
           | Iir_Kind_Simple_Name =>
            Disp_Identifier (Decl);
         when Iir_Kind_Block_Statement
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_Case_Generate_Statement
           | Iir_Kind_For_Generate_Statement =>
            declare
               Ident : constant Name_Id := Get_Label (Decl);
            begin
               if Ident /= Null_Identifier then
                  Disp_Ident (Ident);
               else
                  Put ("<anonymous>");
               end if;
            end;
         when Iir_Kind_Package_Body =>
            Disp_Identifier (Get_Package (Decl));
         when Iir_Kind_Procedure_Body
           | Iir_Kind_Function_Body =>
            Disp_Function_Name (Get_Subprogram_Specification (Decl));
         when Iir_Kind_Protected_Type_Body =>
            Disp_Identifier
              (Get_Type_Declarator (Get_Protected_Type_Declaration (Decl)));
         when others =>
            Error_Kind ("disp_name_of", Decl);
      end case;
   end Disp_Name_Of;

   procedure Disp_Name (Name: Iir) is
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Selected_By_All_Name =>
            Disp_Name (Get_Prefix (Name));
            Put (".all");
         when Iir_Kind_Dereference =>
            Disp_Name (Get_Prefix (Name));
            Put (".all");
         when Iir_Kind_Simple_Name
           | Iir_Kind_Character_Literal =>
            Put (Iirs_Utils.Image_Identifier (Name));
         when Iir_Kind_Operator_Symbol =>
            Disp_Function_Name (Name);
         when Iir_Kind_Selected_Name =>
            Disp_Name (Get_Prefix (Name));
            Put (".");
            Disp_Function_Name (Name);
         when Iir_Kind_Parenthesis_Name =>
            Disp_Name (Get_Prefix (Name));
            Disp_Association_Chain (Get_Association_Chain (Name));
         when Iir_Kind_Base_Attribute =>
            Disp_Name (Get_Prefix (Name));
            Put ("'base");
         when Iir_Kind_Subtype_Attribute =>
            Disp_Name (Get_Prefix (Name));
            Put ("'subtype");
         when Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Unit_Declaration
           | Iir_Kinds_Interface_Object_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Terminal_Declaration
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Group_Template_Declaration =>
            Disp_Name_Of (Name);
         when Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute =>
            Disp_Range (Name);
         when Iir_Kind_Reference_Name =>
            Disp_Name (Get_Referenced_Name (Name));
         when others =>
            Error_Kind ("disp_name", Name);
      end case;
   end Disp_Name;

   procedure Disp_Range (Rng : Iir) is
   begin
      case Get_Kind (Rng) is
         when Iir_Kind_Range_Expression =>
            declare
               Origin : constant Iir := Get_Range_Origin (Rng);
            begin
               if Dump_Origin_Flag and then Origin /= Null_Iir then
                  Disp_Expression (Origin);
               else
                  Disp_Expression (Get_Left_Limit (Rng));
                  if Get_Direction (Rng) = Iir_To then
                     Put (" to ");
                  else
                     Put (" downto ");
                  end if;
                  Disp_Expression (Get_Right_Limit (Rng));
               end if;
            end;
         when Iir_Kind_Range_Array_Attribute =>
            Disp_Parametered_Attribute ("range", Rng);
         when Iir_Kind_Reverse_Range_Array_Attribute =>
            Disp_Parametered_Attribute ("reverse_range", Rng);
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            Disp_Name (Rng);
         when others =>
            Disp_Subtype_Indication (Rng);
            --  Disp_Name_Of (Get_Type_Declarator (Decl));
      end case;
   end Disp_Range;

   procedure Disp_After_End (Decl : Iir; Name : String) is
   begin
      if Get_End_Has_Reserved_Id (Decl) then
         Put (' ');
         Put (Name);
      end if;
      if Get_End_Has_Identifier (Decl) then
         Put (' ');
         Disp_Name_Of (Decl);
      end if;
      Put (';');
      New_Line;
   end Disp_After_End;

   procedure Disp_End (Decl : Iir; Name : String) is
   begin
      Put ("end");
      Disp_After_End (Decl, Name);
   end Disp_End;

   procedure Disp_End_Label (Stmt : Iir; Name : String) is
   begin
      Put ("end");
      Put (' ');
      Put (Name);
      if Get_End_Has_Identifier (Stmt) then
         Put (' ');
         Disp_Ident (Get_Label (Stmt));
      end if;
      Put (';');
      New_Line;
   end Disp_End_Label;

   procedure Disp_Use_Clause (Clause: Iir_Use_Clause)
   is
      Name : Iir;
   begin
      Put ("use ");
      Name := Clause;
      loop
         Disp_Name (Get_Selected_Name (Name));
         Name := Get_Use_Clause_Chain (Name);
         exit when Name = Null_Iir;
         Put (", ");
      end loop;
      Put_Line (";");
   end Disp_Use_Clause;

   -- Disp the resolution function (if any) of type definition DEF.
   procedure Disp_Resolution_Indication (Subtype_Def: Iir)
   is
      procedure Inner (Ind : Iir) is
      begin
         case Get_Kind (Ind) is
            when Iir_Kinds_Denoting_Name =>
               Disp_Name (Ind);
            when Iir_Kind_Array_Element_Resolution =>
               declare
                  Res : constant Iir := Get_Resolution_Indication (Ind);
               begin
                  Put ("(");
                  if Is_Valid (Res) then
                     Inner (Res);
                  else
                     Disp_Name (Get_Resolution_Indication
                                  (Get_Element_Subtype_Indication (Ind)));
                  end if;
                  Put (")");
               end;
            when others =>
               Error_Kind ("disp_resolution_indication", Ind);
         end case;
      end Inner;

      Ind : Iir;
   begin
      case Get_Kind (Subtype_Def) is
         when Iir_Kind_Access_Subtype_Definition =>
            --  No resolution indication on access subtype.
            return;
         when others =>
            Ind := Get_Resolution_Indication (Subtype_Def);
            if Ind = Null_Iir then
               --  No resolution indication.
               return;
            end if;
      end case;

      declare
         Type_Mark : constant Iir := Get_Denoted_Type_Mark (Subtype_Def);
      begin
         if Get_Kind (Type_Mark) in Iir_Kinds_Subtype_Definition
           and then Get_Resolution_Indication (Type_Mark) = Ind
         then
            --  Resolution indication was inherited from the type_mark.
            return;
         end if;
      end;

      Inner (Ind);
      Put (" ");
   end Disp_Resolution_Indication;

   procedure Disp_Integer_Subtype_Definition
     (Def: Iir_Integer_Subtype_Definition)
   is
      Base_Type: Iir_Integer_Type_Definition;
      Decl: Iir;
   begin
      if Def /= Std_Package.Universal_Integer_Subtype_Definition then
         Base_Type := Get_Base_Type (Def);
         Decl := Get_Type_Declarator (Base_Type);
         if Base_Type /= Std_Package.Universal_Integer_Subtype_Definition
           and then Def /= Decl
         then
            Disp_Name_Of (Decl);
            Put (" ");
         end if;
      end if;
      Disp_Resolution_Indication (Def);
      Put ("range ");
      Disp_Expression (Get_Range_Constraint (Def));
      Put (";");
   end Disp_Integer_Subtype_Definition;

   procedure Disp_Floating_Subtype_Definition
     (Def: Iir_Floating_Subtype_Definition)
   is
      Base_Type: Iir_Floating_Type_Definition;
      Decl: Iir;
   begin
      if Def /= Std_Package.Universal_Real_Subtype_Definition then
         Base_Type := Get_Base_Type (Def);
         Decl := Get_Type_Declarator (Base_Type);
         if Base_Type /= Std_Package.Universal_Real_Subtype_Definition
           and then Def /= Decl
         then
            Disp_Name_Of (Decl);
            Put (" ");
         end if;
      end if;
      Disp_Resolution_Indication (Def);
      Put ("range ");
      Disp_Expression (Get_Range_Constraint (Def));
      Put (";");
   end Disp_Floating_Subtype_Definition;

   procedure Disp_Element_Constraint (Def : Iir; Type_Mark : Iir);

   procedure Disp_Array_Element_Constraint (Def : Iir; Type_Mark : Iir)
   is
      Def_El : constant Iir := Get_Element_Subtype (Def);
      Tm_El : constant Iir := Get_Element_Subtype (Type_Mark);
      Has_Index : constant Boolean := Get_Index_Constraint_Flag (Def);
      Has_Own_Element_Subtype : constant Boolean := Def_El /= Tm_El;
      Indexes : Iir_Flist;
      Index : Iir;
   begin
      if not Has_Index and not Has_Own_Element_Subtype then
         return;
      end if;

      if Get_Constraint_State (Type_Mark) /= Fully_Constrained
        and then Has_Index
      then
         Indexes := Get_Index_Subtype_List (Def);
         Put (" (");
         for I in Flist_First .. Flist_Last (Indexes) loop
            Index := Get_Nth_Element (Indexes, I);
            if I /= 0 then
               Put (", ");
            end if;
            --Disp_Expression (Get_Range_Constraint (Index));
            Disp_Range (Index);
         end loop;
         Put (")");
      end if;

      if Has_Own_Element_Subtype
        and then Get_Kind (Def_El) in Iir_Kinds_Composite_Type_Definition
      then
         Disp_Element_Constraint (Def_El, Tm_El);
      end if;
   end Disp_Array_Element_Constraint;

   procedure Disp_Record_Element_Constraint (Def : Iir)
   is
      El_List : constant Iir_Flist := Get_Elements_Declaration_List (Def);
      El : Iir;
      Has_El : Boolean := False;
   begin
      for I in Flist_First .. Flist_Last (El_List) loop
         El := Get_Nth_Element (El_List, I);
         if Get_Kind (El) = Iir_Kind_Record_Element_Constraint
           and then Get_Parent (El) = Def
         then
            if Has_El then
               Put (", ");
            else
               Put ("(");
               Has_El := True;
            end if;
            Disp_Name_Of (El);
            Disp_Element_Constraint (Get_Type (El),
                                     Get_Base_Type (Get_Type (El)));
         end if;
      end loop;
      if Has_El then
         Put (")");
      end if;
   end Disp_Record_Element_Constraint;

   procedure Disp_Element_Constraint (Def : Iir; Type_Mark : Iir) is
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Record_Subtype_Definition =>
            Disp_Record_Element_Constraint (Def);
         when Iir_Kind_Array_Subtype_Definition =>
            Disp_Array_Element_Constraint (Def, Type_Mark);
         when others =>
            Error_Kind ("disp_element_constraint", Def);
      end case;
   end Disp_Element_Constraint;

   procedure Disp_Tolerance_Opt (N : Iir) is
      Tol : constant Iir := Get_Tolerance (N);
   begin
      if Tol /= Null_Iir then
         Put ("tolerance ");
         Disp_Expression (Tol);
      end if;
   end Disp_Tolerance_Opt;

   procedure Disp_Subtype_Indication (Def : Iir; Full_Decl : Boolean := False)
   is
      Type_Mark : Iir;
      Base_Type : Iir;
      Decl : Iir;
   begin
      case Get_Kind (Def) is
         when Iir_Kinds_Denoting_Name
           | Iir_Kind_Subtype_Attribute =>
            Disp_Name (Def);
            return;
         when others =>
            null;
      end case;

      Decl := Get_Type_Declarator (Def);
      if not Full_Decl and then Decl /= Null_Iir then
         Disp_Name_Of (Decl);
         return;
      end if;

      -- Resolution function name.
      Disp_Resolution_Indication (Def);

      -- type mark.
      Type_Mark := Get_Subtype_Type_Mark (Def);
      if Type_Mark /= Null_Iir then
         Disp_Name (Type_Mark);
         Type_Mark := Get_Type (Type_Mark);
      end if;

      Base_Type := Get_Base_Type (Def);
      case Get_Kind (Base_Type) is
         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Type_Definition =>
            if Type_Mark = Null_Iir
              or else Get_Range_Constraint (Def)
              /= Get_Range_Constraint (Type_Mark)
            then
               if Type_Mark /= Null_Iir then
                  Put (" range ");
               end if;
               Disp_Expression (Get_Range_Constraint (Def));
            end if;
            if Get_Kind (Base_Type) = Iir_Kind_Floating_Type_Definition then
               Disp_Tolerance_Opt (Def);
            end if;
         when Iir_Kind_Access_Type_Definition =>
            declare
               Des_Ind : constant Iir :=
                 Get_Designated_Subtype_Indication (Def);
            begin
               if Des_Ind /= Null_Iir then
                  pragma Assert
                    (Get_Kind (Des_Ind) = Iir_Kind_Array_Subtype_Definition);
                  Disp_Array_Element_Constraint
                    (Des_Ind, Get_Designated_Type (Base_Type));
               end if;
            end;
         when Iir_Kind_Array_Type_Definition =>
            if Type_Mark = Null_Iir then
               Disp_Array_Element_Constraint (Def, Def);
            else
               Disp_Array_Element_Constraint (Def, Type_Mark);
            end if;
         when Iir_Kind_Record_Type_Definition =>
            Disp_Record_Element_Constraint (Def);
         when others =>
            Error_Kind ("disp_subtype_indication", Base_Type);
      end case;
   end Disp_Subtype_Indication;

   procedure Disp_Enumeration_Type_Definition
     (Def: Iir_Enumeration_Type_Definition)
   is
      Lits : constant Iir_Flist := Get_Enumeration_Literal_List (Def);
      Len : Count;
      Start_Col: Count;
      Decl: Name_Id;
      A_Lit: Iir; --Enumeration_Literal_Acc;
   begin
      for I in Flist_First .. Flist_Last (Lits) loop
         A_Lit := Get_Nth_Element (Lits, I);
         if I = 0 then
            Put ("(");
            Start_Col := Col;
         else
            Put (", ");
         end if;
         Decl := Get_Identifier (A_Lit);
         if Name_Table.Is_Character (Decl) then
            Len := 3;
         else
            Len := Count (Name_Table.Get_Name_Length (Decl));
         end if;
         if Col + Len + 2 > Line_Length then
            New_Line;
            Set_Col (Start_Col);
         end if;
         Disp_Name_Of (A_Lit);
      end loop;
      Put (");");
   end Disp_Enumeration_Type_Definition;

   procedure Disp_Enumeration_Subtype_Definition
     (Def: Iir_Enumeration_Subtype_Definition)
   is
   begin
      Disp_Resolution_Indication (Def);
      Put ("range ");
      Disp_Range (Def);
      Put (";");
   end Disp_Enumeration_Subtype_Definition;

   procedure Disp_Discrete_Range (Iterator: Iir) is
   begin
      if Get_Kind (Iterator) in Iir_Kinds_Subtype_Definition then
         Disp_Subtype_Indication (Iterator);
      else
         Disp_Range (Iterator);
      end if;
   end Disp_Discrete_Range;

   procedure Disp_Array_Subtype_Definition (Def: Iir_Array_Subtype_Definition)
   is
      Indexes : constant Iir_Flist := Get_Index_Subtype_List (Def);
      Index: Iir;
   begin
      Disp_Resolution_Indication (Def);

      Put ("array (");
      for I in Flist_First .. Flist_Last (Indexes) loop
         Index := Get_Nth_Element (Indexes, I);
         if I /= 0 then
            Put (", ");
         end if;
         Disp_Discrete_Range (Index);
      end loop;
      Put (") of ");
      Disp_Subtype_Indication (Get_Element_Subtype (Def));
   end Disp_Array_Subtype_Definition;

   procedure Disp_Array_Type_Definition (Def: Iir_Array_Type_Definition)
   is
      Indexes : constant Iir_Flist := Get_Index_Subtype_List (Def);
      Index: Iir;
   begin
      Put ("array (");
      for I in Flist_First .. Flist_Last (Indexes) loop
         Index := Get_Nth_Element (Indexes, I);
         if I /= 0 then
            Put (", ");
         end if;
         Disp_Name (Index);
         Put (" range <>");
      end loop;
      Put (") of ");
      Disp_Subtype_Indication (Get_Element_Subtype_Indication (Def));
      Put (";");
   end Disp_Array_Type_Definition;

   procedure Disp_Physical_Literal (Lit: Iir)
   is
      Unit : Iir;
   begin
      case Get_Kind (Lit) is
         when Iir_Kind_Physical_Int_Literal =>
            Disp_Int64 (Get_Value (Lit));
         when Iir_Kind_Physical_Fp_Literal =>
            Disp_Fp64 (Get_Fp_Value (Lit));
         when Iir_Kind_Unit_Declaration =>
            Disp_Identifier (Lit);
            return;
         when others =>
            Error_Kind ("disp_physical_literal", Lit);
      end case;
      Put (' ');

      Unit := Get_Unit_Name (Lit);
      if Is_Valid (Unit) then
         --  No unit in range_constraint of physical type declaration.
         Disp_Name (Unit);
      end if;
   end Disp_Physical_Literal;

   procedure Disp_Physical_Subtype_Definition
     (Def: Iir_Physical_Subtype_Definition) is
   begin
      Disp_Resolution_Indication (Def);
      Put ("range ");
      Disp_Expression (Get_Range_Constraint (Def));
   end Disp_Physical_Subtype_Definition;

   procedure Disp_Record_Type_Definition
     (Def: Iir_Record_Type_Definition; Indent: Count)
   is
      List : constant Iir_Flist := Get_Elements_Declaration_List (Def);
      El: Iir_Element_Declaration;
      Reindent : Boolean;
   begin
      Put_Line ("record");
      Set_Col (Indent);
      Reindent := True;
      for I in Flist_First .. Flist_Last (List) loop
         El := Get_Nth_Element (List, I);
         if Reindent then
            Set_Col (Indent + Indentation);
         end if;
         Disp_Identifier (El);
         if Get_Has_Identifier_List (El) then
            Put (", ");
            Reindent := False;
         else
            Put (" : ");
            Disp_Subtype_Indication (Get_Type (El));
            Put_Line (";");
            Reindent := True;
         end if;
      end loop;
      Set_Col (Indent);
      Disp_End (Def, "record");
   end Disp_Record_Type_Definition;

   procedure Disp_Designator_List (List: Iir_List)
   is
      El : Iir;
      It : List_Iterator;
      Is_First : Boolean;
   begin
      case List is
         when Null_Iir_List =>
            null;
         when Iir_List_All =>
            Put ("all");
         when others =>
            It := List_Iterate (List);
            Is_First := True;
            while Is_Valid (It) loop
               El := Get_Element (It);
               if not Is_First then
                  Put (", ");
               else
                  Is_First := False;
               end if;
               Disp_Expression (El);
               Next (It);
            end loop;
      end case;
   end Disp_Designator_List;

   -- Display the full definition of a type, ie the sequence that can create
   -- such a type.
   procedure Disp_Type_Definition (Def: Iir; Indent: Count) is
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Enumeration_Type_Definition =>
            Disp_Enumeration_Type_Definition (Def);
         when Iir_Kind_Enumeration_Subtype_Definition =>
            Disp_Enumeration_Subtype_Definition (Def);
         when Iir_Kind_Integer_Subtype_Definition =>
            Disp_Integer_Subtype_Definition (Def);
         when Iir_Kind_Floating_Subtype_Definition =>
            Disp_Floating_Subtype_Definition (Def);
         when Iir_Kind_Array_Type_Definition =>
            Disp_Array_Type_Definition (Def);
         when Iir_Kind_Array_Subtype_Definition =>
            Disp_Array_Subtype_Definition (Def);
         when Iir_Kind_Physical_Subtype_Definition =>
            Disp_Physical_Subtype_Definition (Def);
         when Iir_Kind_Record_Type_Definition =>
            Disp_Record_Type_Definition (Def, Indent);
         when Iir_Kind_Access_Type_Definition =>
            Put ("access ");
            Disp_Subtype_Indication (Get_Designated_Subtype_Indication (Def));
            Put (';');
         when Iir_Kind_File_Type_Definition =>
            Put ("file of ");
            Disp_Subtype_Indication (Get_File_Type_Mark (Def));
            Put (';');
         when Iir_Kind_Protected_Type_Declaration =>
            Put_Line ("protected");
            Disp_Declaration_Chain (Def, Indent + Indentation);
            Set_Col (Indent);
            Disp_End (Def, "protected");
         when Iir_Kind_Integer_Type_Definition =>
            Put ("<integer base type>");
         when Iir_Kind_Floating_Type_Definition =>
            Put ("<floating base type>");
         when Iir_Kind_Physical_Type_Definition =>
            Put ("<physical base type>");
         when others =>
            Error_Kind ("disp_type_definition", Def);
      end case;
   end Disp_Type_Definition;

   procedure Disp_Type_Declaration (Decl: Iir_Type_Declaration)
   is
      Indent : constant Count := Col;
      Def : constant Iir := Get_Type_Definition (Decl);
   begin
      Put ("type ");
      Disp_Name_Of (Decl);
      if Def = Null_Iir
        or else Get_Kind (Def) = Iir_Kind_Incomplete_Type_Definition
      then
         Put_Line (";");
      else
         Put (" is ");
         Disp_Type_Definition (Def, Indent);
         New_Line;
      end if;
   end Disp_Type_Declaration;

   procedure Disp_Anonymous_Type_Declaration
     (Decl: Iir_Anonymous_Type_Declaration)
   is
      Def : constant Iir := Get_Type_Definition (Decl);
      Indent: constant Count := Col;
   begin
      Put ("type ");
      Disp_Identifier (Decl);
      Put (" is ");
      case Get_Kind (Def) is
         when Iir_Kind_Array_Type_Definition =>
            declare
               St : constant Iir := Get_Subtype_Definition (Decl);
               Indexes : constant Iir_Flist := Get_Index_Subtype_List (St);
               Index : Iir;
            begin
               Put ("array (");
               for I in Flist_First .. Flist_Last (Indexes) loop
                  Index := Get_Nth_Element (Indexes, I);
                  if I /= 0 then
                     Put (", ");
                  end if;
                  Disp_Discrete_Range (Index);
               end loop;
               Put (") of ");
               Disp_Subtype_Indication (Get_Element_Subtype_Indication (Def));
               Put (";");
            end;
         when Iir_Kind_Physical_Type_Definition =>
            declare
               St : constant Iir := Get_Subtype_Definition (Decl);
               Unit : Iir_Unit_Declaration;
            begin
               Put ("range ");
               Disp_Expression (Get_Range_Constraint (St));
               Put_Line (" units");
               Set_Col (Indent + Indentation);
               Unit := Get_Unit_Chain (Def);
               Disp_Identifier (Unit);
               Put_Line (";");
               Unit := Get_Chain (Unit);
               while Unit /= Null_Iir loop
                  Set_Col (Indent + Indentation);
                  Disp_Identifier (Unit);
                  Put (" = ");
                  Disp_Expression (Get_Physical_Literal (Unit));
                  Put_Line (";");
                  Unit := Get_Chain (Unit);
               end loop;
               Set_Col (Indent);
               Disp_End (Def, "units");
            end;
         when Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Integer_Type_Definition =>
            declare
               St : constant Iir := Get_Subtype_Definition (Decl);
            begin
               Put ("range ");
               Disp_Expression (Get_Range_Constraint (St));
               Put (";");
            end;
         when others =>
            Disp_Type_Definition (Def, Indent);
      end case;
      New_Line;
   end Disp_Anonymous_Type_Declaration;

   procedure Disp_Subtype_Declaration (Decl: in Iir_Subtype_Declaration)
   is
      Def : constant Iir := Get_Type (Decl);
      Bt_Decl : constant Iir := Get_Type_Declarator (Get_Base_Type (Def));
   begin
      if Get_Identifier (Decl) = Get_Identifier (Bt_Decl) then
         Put ("-- ");
      end if;
      Put ("subtype ");
      Disp_Name_Of (Decl);
      Put (" is ");
      Disp_Subtype_Indication (Def, True);
      Put_Line (";");
   end Disp_Subtype_Declaration;

   procedure Disp_Type (A_Type: Iir)
   is
      Decl: Iir;
   begin
      Decl := Get_Type_Declarator (A_Type);
      if Decl /= Null_Iir then
         Disp_Name_Of (Decl);
      else
         case Get_Kind (A_Type) is
            when Iir_Kind_Enumeration_Type_Definition
              | Iir_Kind_Integer_Type_Definition =>
               raise Program_Error;
            when Iir_Kind_Integer_Subtype_Definition
              | Iir_Kind_Enumeration_Subtype_Definition
              | Iir_Kind_Access_Subtype_Definition =>
               Disp_Subtype_Indication (A_Type);
            when Iir_Kind_Array_Subtype_Definition =>
               Disp_Subtype_Indication (A_Type);
            when others =>
               Error_Kind ("disp_type", A_Type);
         end case;
      end if;
   end Disp_Type;

   procedure Disp_Nature_Definition (Def : Iir) is
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Scalar_Nature_Definition =>
            Disp_Subtype_Indication (Get_Across_Type (Def));
            Put (" across ");
            Disp_Subtype_Indication (Get_Through_Type (Def));
            Put (" through ");
            Disp_Name_Of (Get_Reference (Def));
            Put (" reference");
         when others =>
            Error_Kind ("disp_nature_definition", Def);
      end case;
   end Disp_Nature_Definition;

   procedure Disp_Nature_Declaration (Decl : Iir) is
   begin
      Put ("nature ");
      Disp_Name_Of (Decl);
      Put (" is ");
      Disp_Nature_Definition (Get_Nature (Decl));
      Put_Line (";");
   end Disp_Nature_Declaration;

   procedure Disp_Nature (Nature : Iir)
   is
      Decl: Iir;
   begin
      Decl := Get_Nature_Declarator (Nature);
      if Decl /= Null_Iir then
         Disp_Name_Of (Decl);
      else
         Error_Kind ("disp_nature", Nature);
      end if;
   end Disp_Nature;

   procedure Disp_Mode (Mode: Iir_Mode) is
   begin
      case Mode is
         when Iir_In_Mode =>
            Put ("in ");
         when Iir_Out_Mode =>
            Put ("out ");
         when Iir_Inout_Mode =>
            Put ("inout ");
         when Iir_Buffer_Mode =>
            Put ("buffer ");
         when Iir_Linkage_Mode =>
            Put ("linkage ");
         when Iir_Unknown_Mode =>
            Put ("<unknown> ");
      end case;
   end Disp_Mode;

   procedure Disp_Signal_Kind (Sig : Iir) is
   begin
      if Get_Guarded_Signal_Flag (Sig) then
         case Get_Signal_Kind (Sig) is
            when Iir_Register_Kind =>
               Put (" register");
            when Iir_Bus_Kind =>
               Put (" bus");
         end case;
      end if;
   end Disp_Signal_Kind;

   procedure Disp_Interface_Class (Inter: Iir) is
   begin
      if Get_Has_Class (Inter) then
         case Get_Kind (Inter) is
            when Iir_Kind_Interface_Signal_Declaration =>
               Put ("signal ");
            when Iir_Kind_Interface_Variable_Declaration =>
               Put ("variable ");
            when Iir_Kind_Interface_Constant_Declaration =>
               Put ("constant ");
            when Iir_Kind_Interface_File_Declaration =>
               Put ("file ");
            when others =>
               Error_Kind ("disp_interface_class", Inter);
         end case;
      end if;
   end Disp_Interface_Class;

   procedure Disp_Interface_Mode_And_Type (Inter: Iir)
   is
      Default: constant Iir := Get_Default_Value (Inter);
      Ind : constant Iir := Get_Subtype_Indication (Inter);
   begin
      Put (": ");
      if Get_Has_Mode (Inter) then
         Disp_Mode (Get_Mode (Inter));
      end if;
      if Ind = Null_Iir then
         --  For implicit subprogram
         Disp_Type (Get_Type (Inter));
      else
         Disp_Subtype_Indication (Get_Subtype_Indication (Inter));
      end if;
      if Get_Kind (Inter) = Iir_Kind_Interface_Signal_Declaration then
         Disp_Signal_Kind (Inter);
      end if;
      if Default /= Null_Iir then
         Put (" := ");
         Disp_Expression (Default);
      end if;
   end Disp_Interface_Mode_And_Type;

   --  Disp interfaces, followed by END_STR (';' in general).
   procedure Disp_Interface_Chain (Chain: Iir;
                                   End_Str: String := "";
                                   Comment_Col : Natural := 0)
   is
      Inter: Iir;
      Next_Inter : Iir;
      First_Inter : Iir;
      Start: Count;
   begin
      if Chain = Null_Iir then
         return;
      end if;
      Put (" (");
      Start := Col;
      Inter := Chain;
      loop
         Next_Inter := Get_Chain (Inter);
         Set_Col (Start);

         First_Inter := Inter;

         case Get_Kind (Inter) is
            when Iir_Kinds_Interface_Object_Declaration =>
               Disp_Interface_Class (Inter);
               Disp_Name_Of (Inter);
               while Get_Has_Identifier_List (Inter) loop
                  Put (", ");
                  Inter := Next_Inter;
                  Next_Inter := Get_Chain (Inter);
                  Disp_Name_Of (Inter);
               end loop;
               Disp_Interface_Mode_And_Type (First_Inter);
            when Iir_Kind_Interface_Package_Declaration =>
               Put ("package ");
               Disp_Identifier (Inter);
               Put (" is new ");
               Disp_Name (Get_Uninstantiated_Package_Name (Inter));
               Put (" generic map ");
               declare
                  Assoc_Chain : constant Iir :=
                    Get_Generic_Map_Aspect_Chain (Inter);
               begin
                  if Assoc_Chain = Null_Iir then
                     Put ("(<>)");
                  else
                     Disp_Association_Chain (Assoc_Chain);
                  end if;
               end;
            when Iir_Kind_Interface_Type_Declaration =>
               Put ("type ");
               Disp_Identifier (Inter);
            when Iir_Kinds_Interface_Subprogram_Declaration =>
               Disp_Subprogram_Declaration (Inter);
            when others =>
               Error_Kind ("disp_interface_chain", Inter);
         end case;

         if Next_Inter /= Null_Iir then
            Put (";");
            if Comment_Col /= 0 then
               New_Line;
               Set_Col (Comment_Col);
               Put ("--");
            end if;
         else
            Put (')');
            Put (End_Str);
            exit;
         end if;

         Inter := Next_Inter;
         Next_Inter := Get_Chain (Inter);
      end loop;
   end Disp_Interface_Chain;

   procedure Disp_Ports (Parent : Iir) is
   begin
      Put ("port");
      Disp_Interface_Chain (Get_Port_Chain (Parent), ";");
   end Disp_Ports;

   procedure Disp_Generics (Parent : Iir) is
   begin
      Put ("generic");
      Disp_Interface_Chain (Get_Generic_Chain (Parent), ";");
   end Disp_Generics;

   procedure Disp_Entity_Declaration (Decl: Iir_Entity_Declaration)
   is
      Start: constant Count := Col;
   begin
      Put ("entity ");
      Disp_Name_Of (Decl);
      Put_Line (" is");
      if Get_Generic_Chain (Decl) /= Null_Iir then
         Set_Col (Start + Indentation);
         Disp_Generics (Decl);
      end if;
      if Get_Port_Chain (Decl) /= Null_Iir then
         Set_Col (Start + Indentation);
         Disp_Ports (Decl);
      end if;
      Disp_Declaration_Chain (Decl, Start + Indentation);
      if Get_Has_Begin (Decl) then
         Set_Col (Start);
         Put_Line ("begin");
      end if;
      if Get_Concurrent_Statement_Chain (Decl) /= Null_Iir then
         Disp_Concurrent_Statement_Chain (Decl, Start + Indentation);
      end if;
      Set_Col (Start);
      Disp_End (Decl, "entity");
   end Disp_Entity_Declaration;

   procedure Disp_Component_Declaration (Decl: Iir_Component_Declaration)
   is
      Indent: Count;
   begin
      Indent := Col;
      Put ("component ");
      Disp_Name_Of (Decl);
      if Get_Has_Is (Decl) then
         Put (" is");
      end if;
      if Get_Generic_Chain (Decl) /= Null_Iir then
         Set_Col (Indent + Indentation);
         Disp_Generics (Decl);
      end if;
      if Get_Port_Chain (Decl) /= Null_Iir then
         Set_Col (Indent + Indentation);
         Disp_Ports (Decl);
      end if;
      Set_Col (Indent);
      Disp_End (Decl, "component");
   end Disp_Component_Declaration;

   procedure Disp_Concurrent_Statement_Chain (Parent : Iir; Indent : Count)
   is
      El: Iir;
   begin
      El := Get_Concurrent_Statement_Chain (Parent);
      while El /= Null_Iir loop
         Set_Col (Indent);
         Disp_Concurrent_Statement (El);
         El := Get_Chain (El);
      end loop;
   end Disp_Concurrent_Statement_Chain;

   procedure Disp_Architecture_Body (Arch: Iir_Architecture_Body)
   is
      Start: Count;
   begin
      Start := Col;
      Put ("architecture ");
      Disp_Name_Of (Arch);
      Put (" of ");
      Disp_Name (Get_Entity_Name (Arch));
      Put_Line (" is");
      Disp_Declaration_Chain (Arch, Start + Indentation);
      Set_Col (Start);
      Put_Line ("begin");
      Disp_Concurrent_Statement_Chain (Arch, Start + Indentation);
      Set_Col (Start);
      Disp_End (Arch, "architecture");
   end Disp_Architecture_Body;

   procedure Disp_Signature (Sig : Iir)
   is
      Prefix : constant Iir := Get_Signature_Prefix (Sig);
      List : constant Iir_Flist := Get_Type_Marks_List (Sig);
      El : Iir;
   begin
      if Is_Valid (Prefix) then
         --  Only in alias.
         Disp_Name (Prefix);
      end if;
      Put (" [");
      if List /= Null_Iir_Flist then
         for I in Flist_First .. Flist_Last (List) loop
            El := Get_Nth_Element (List, I);
            if I /= 0 then
               Put (", ");
            end if;
            Disp_Name (El);
         end loop;
      end if;
      El := Get_Return_Type_Mark (Sig);
      if El /= Null_Iir then
         Put (" return ");
         Disp_Name (El);
      end if;
      Put ("]");
   end Disp_Signature;

   procedure Disp_Object_Alias_Declaration (Decl: Iir_Object_Alias_Declaration)
   is
   begin
      Put ("alias ");
      Disp_Name_Of (Decl);
      Put (": ");
      Disp_Type (Get_Type (Decl));
      Put (" is ");
      Disp_Expression (Get_Name (Decl));
      Put_Line (";");
   end Disp_Object_Alias_Declaration;

   procedure Disp_Non_Object_Alias_Declaration
     (Decl: Iir_Non_Object_Alias_Declaration)
   is
      Sig : constant Iir := Get_Alias_Signature (Decl);
   begin
      if Get_Implicit_Alias_Flag (Decl) then
         Put ("-- ");
      end if;

      Put ("alias ");
      Disp_Function_Name (Decl);
      Put (" is ");
      Disp_Name (Get_Name (Decl));
      if Sig /= Null_Iir then
         Disp_Signature (Sig);
      end if;
      Put_Line (";");
   end Disp_Non_Object_Alias_Declaration;

   procedure Disp_File_Declaration (Decl: Iir_File_Declaration)
   is
      Next_Decl : Iir;
      Expr: Iir;
   begin
      Put ("file ");
      Disp_Name_Of (Decl);
      Next_Decl := Decl;
      while Get_Has_Identifier_List (Next_Decl) loop
         Next_Decl := Get_Chain (Next_Decl);
         Put (", ");
         Disp_Name_Of (Next_Decl);
      end loop;
      Put (": ");
      Disp_Type (Get_Type (Decl));
      if Vhdl_Std = Vhdl_87 then
         Put (" is ");
         if Get_Has_Mode (Decl) then
            Disp_Mode (Get_Mode (Decl));
         end if;
         Disp_Expression (Get_File_Logical_Name (Decl));
      else
         Expr := Get_File_Open_Kind (Decl);
         if Expr /= Null_Iir then
            Put (" open ");
            Disp_Expression (Expr);
         end if;
         Expr := Get_File_Logical_Name (Decl);
         if Expr /= Null_Iir then
            Put (" is ");
            Disp_Expression (Expr);
         end if;
      end if;
      Put (';');
   end Disp_File_Declaration;

   procedure Disp_Quantity_Declaration (Decl: Iir)
   is
      Expr : Iir;
      Term : Iir;
   begin
      Put ("quantity ");
      Disp_Name_Of (Decl);

      case Get_Kind (Decl) is
         when Iir_Kinds_Branch_Quantity_Declaration =>
            Disp_Tolerance_Opt (Decl);
            Expr := Get_Default_Value (Decl);
            if Expr /= Null_Iir then
               Put (":= ");
               Disp_Expression (Expr);
            end if;
            if Get_Kind (Decl) = Iir_Kind_Across_Quantity_Declaration then
               Put (" across ");
            else
               Put (" through ");
            end if;
            Disp_Name_Of (Get_Plus_Terminal (Decl));
            Term := Get_Minus_Terminal (Decl);
            if Term /= Null_Iir then
               Put (" to ");
               Disp_Name_Of (Term);
            end if;
         when Iir_Kind_Free_Quantity_Declaration =>
            Put (": ");
            Disp_Type (Get_Type (Decl));
            Expr := Get_Default_Value (Decl);
            if Expr /= Null_Iir then
               Put (":= ");
               Disp_Expression (Expr);
            end if;
         when others =>
            raise Program_Error;
      end case;
      Put (';');
   end Disp_Quantity_Declaration;

   procedure Disp_Terminal_Declaration (Decl: Iir) is
   begin
      Put ("terminal ");
      Disp_Name_Of (Decl);
      Put (": ");
      Disp_Nature (Get_Nature (Decl));
      Put (';');
   end Disp_Terminal_Declaration;

   procedure Disp_Object_Declaration (Decl: Iir)
   is
      Next_Decl : Iir;
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Variable_Declaration =>
            if Get_Shared_Flag (Decl) then
               Put ("shared ");
            end if;
            Put ("variable ");
         when Iir_Kind_Constant_Declaration =>
            Put ("constant ");
         when Iir_Kind_Signal_Declaration =>
            Put ("signal ");
         when Iir_Kind_File_Declaration =>
            Disp_File_Declaration (Decl);
            return;
         when others =>
            raise Internal_Error;
      end case;
      Disp_Name_Of (Decl);
      Next_Decl := Decl;
      while Get_Has_Identifier_List (Next_Decl) loop
         Next_Decl := Get_Chain (Next_Decl);
         Put (", ");
         Disp_Name_Of (Next_Decl);
      end loop;
      Put (": ");
      Disp_Subtype_Indication (Get_Subtype_Indication (Decl));
      if Get_Kind (Decl) = Iir_Kind_Signal_Declaration then
         Disp_Signal_Kind (Decl);
      end if;

      if Get_Default_Value (Decl) /= Null_Iir then
         Put (" := ");
         Disp_Expression (Get_Default_Value (Decl));
      end if;
      Put_Line (";");
   end Disp_Object_Declaration;

   procedure Disp_Pure (Subprg : Iir) is
   begin
      if Get_Pure_Flag (Subprg) then
         Put ("pure");
      else
         Put ("impure");
      end if;
   end Disp_Pure;

   procedure Disp_Subprogram_Declaration (Subprg: Iir)
   is
      Start : constant Count := Col;
      Implicit : constant Boolean := Is_Implicit_Subprogram (Subprg);
      Inter : Iir;
   begin
      if Implicit
        and then
        Get_Implicit_Definition (Subprg) /= Iir_Predefined_Now_Function
      then
         Put ("-- ");
      end if;

      case Get_Kind (Subprg) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Interface_Function_Declaration =>
            if Get_Has_Pure (Subprg) then
               Disp_Pure (Subprg);
               Put (' ');
            end if;
            Put ("function");
         when Iir_Kind_Procedure_Declaration
           | Iir_Kind_Interface_Procedure_Declaration =>
            Put ("procedure");
         when others =>
            raise Internal_Error;
      end case;

      Put (' ');
      Disp_Function_Name (Subprg);

      if Get_Has_Parameter (Subprg) then
         Put (' ');
         Put ("parameter");
      end if;

      Inter := Get_Interface_Declaration_Chain (Subprg);
      if Implicit then
         Disp_Interface_Chain (Inter, "", Start);
      else
         Disp_Interface_Chain (Inter, "", 0);
      end if;

      case Get_Kind (Subprg) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Interface_Function_Declaration =>
            Put (" return ");
            if Implicit then
               Disp_Type (Get_Return_Type (Subprg));
            else
               Disp_Name (Get_Return_Type_Mark (Subprg));
            end if;
         when Iir_Kind_Procedure_Declaration
           | Iir_Kind_Interface_Procedure_Declaration =>
            null;
         when others =>
            raise Internal_Error;
      end case;
   end Disp_Subprogram_Declaration;

   procedure Disp_Subprogram_Body (Subprg : Iir)
   is
      Indent : constant Count := Col;
   begin
      Disp_Declaration_Chain (Subprg, Indent + Indentation);
      Set_Col (Indent);
      Put_Line ("begin");
      Set_Col (Indent + Indentation);
      Disp_Sequential_Statements (Get_Sequential_Statement_Chain (Subprg));
      Set_Col (Indent);
      if Get_Kind (Subprg) = Iir_Kind_Function_Body then
         Disp_End (Subprg, "function");
      else
         Disp_End (Subprg, "procedure");
      end if;
   end Disp_Subprogram_Body;

   procedure Disp_Instantiation_List (Insts: Iir_Flist) is
      El : Iir;
   begin
      if Insts = Iir_Flist_All then
         Put ("all");
      elsif Insts = Iir_Flist_Others then
         Put ("others");
      else
         for I in Flist_First .. Flist_Last (Insts) loop
            El := Get_Nth_Element (Insts, I);
            if I /= Flist_First then
               Put (", ");
            end if;
            Disp_Name (El);
         end loop;
      end if;
   end Disp_Instantiation_List;

   procedure Disp_Configuration_Specification
     (Spec : Iir_Configuration_Specification)
   is
      Indent : Count;
   begin
      Indent := Col;
      Put ("for ");
      Disp_Instantiation_List (Get_Instantiation_List (Spec));
      Put (": ");
      Disp_Name (Get_Component_Name (Spec));
      New_Line;
      Disp_Binding_Indication (Get_Binding_Indication (Spec),
                               Indent + Indentation);
      Put_Line (";");
   end Disp_Configuration_Specification;

   procedure Disp_Disconnection_Specification
     (Dis : Iir_Disconnection_Specification)
   is
   begin
      Put ("disconnect ");
      Disp_Instantiation_List (Get_Signal_List (Dis));
      Put (": ");
      Disp_Name (Get_Type_Mark (Dis));
      Put (" after ");
      Disp_Expression (Get_Expression (Dis));
      Put_Line (";");
   end Disp_Disconnection_Specification;

   procedure Disp_Attribute_Declaration (Attr : Iir_Attribute_Declaration)
   is
   begin
      Put ("attribute ");
      Disp_Identifier (Attr);
      Put (": ");
      Disp_Name (Get_Type_Mark (Attr));
      Put_Line (";");
   end Disp_Attribute_Declaration;

   procedure Disp_Attribute_Value (Attr : Iir) is
   begin
      Disp_Name_Of (Get_Designated_Entity (Attr));
      Put ("'");
      Disp_Identifier
        (Get_Attribute_Designator (Get_Attribute_Specification (Attr)));
   end Disp_Attribute_Value;

   procedure Disp_Attribute_Name (Attr : Iir)
   is
      Sig : constant Iir := Get_Attribute_Signature (Attr);
   begin
      Disp_Name (Get_Prefix (Attr));
      if Sig /= Null_Iir then
         Disp_Signature (Sig);
      end if;
      Put ("'");
      Disp_Ident (Get_Identifier (Attr));
   end Disp_Attribute_Name;

   procedure Disp_Entity_Kind (Tok : Tokens.Token_Type) is
   begin
      Put (Tokens.Image (Tok));
   end Disp_Entity_Kind;

   procedure Disp_Entity_Name_List (List : Iir_Flist)
   is
      El : Iir;
   begin
      if List = Iir_Flist_All then
         Put ("all");
      elsif List = Iir_Flist_Others then
         Put ("others");
      else
         for I in Flist_First .. Flist_Last (List) loop
            El := Get_Nth_Element (List, I);
            if I /= Flist_First then
               Put (", ");
            end if;
            if Get_Kind (El) = Iir_Kind_Signature then
               Disp_Signature (El);
            else
               Disp_Name (El);
            end if;
         end loop;
      end if;
   end Disp_Entity_Name_List;

   procedure Disp_Attribute_Specification (Attr : Iir_Attribute_Specification)
   is
   begin
      Put ("attribute ");
      Disp_Identifier (Get_Attribute_Designator (Attr));
      Put (" of ");
      Disp_Entity_Name_List (Get_Entity_Name_List (Attr));
      Put (": ");
      Disp_Entity_Kind (Get_Entity_Class (Attr));
      Put (" is ");
      Disp_Expression (Get_Expression (Attr));
      Put_Line (";");
   end Disp_Attribute_Specification;

   procedure Disp_Protected_Type_Body
     (Bod : Iir_Protected_Type_Body; Indent : Count)
   is
   begin
      Put ("type ");
      Disp_Identifier (Bod);
      Put (" is protected body");
      New_Line;
      Disp_Declaration_Chain (Bod, Indent + Indentation);
      Set_Col (Indent);
      Disp_End (Bod, "protected body");
   end Disp_Protected_Type_Body;

   procedure Disp_Group_Template_Declaration (Decl : Iir)
   is
      use Tokens;
      Ent : Iir;
   begin
      Put ("group ");
      Disp_Identifier (Decl);
      Put (" is (");
      Ent := Get_Entity_Class_Entry_Chain (Decl);
      loop
         Disp_Entity_Kind (Get_Entity_Class (Ent));
         Ent := Get_Chain (Ent);
         exit when Ent = Null_Iir;
         if Get_Entity_Class (Ent) = Tok_Box then
            Put (" <>");
            exit;
         else
            Put (", ");
         end if;
      end loop;
      Put_Line (");");
   end Disp_Group_Template_Declaration;

   procedure Disp_Group_Declaration (Decl : Iir)
   is
      List : Iir_Flist;
      El : Iir;
   begin
      Put ("group ");
      Disp_Identifier (Decl);
      Put (" : ");
      Disp_Name (Get_Group_Template_Name (Decl));
      Put (" (");
      List := Get_Group_Constituent_List (Decl);
      for I in Flist_First .. Flist_Last (List) loop
         El := Get_Nth_Element (List, I);
         if I /= 0 then
            Put (", ");
         end if;
         Disp_Name_Of (El);
      end loop;
      Put_Line (");");
   end Disp_Group_Declaration;

   procedure Disp_PSL_HDL_Expr (N : PSL.Nodes.HDL_Node) is
   begin
      Disp_Expression (Iir (N));
   end Disp_PSL_HDL_Expr;

   procedure Disp_Psl_Expression (Expr : PSL_Node) is
   begin
      PSL.Prints.HDL_Expr_Printer := Disp_PSL_HDL_Expr'Access;
      PSL.Prints.Print_Property (Expr);
   end Disp_Psl_Expression;

   procedure Disp_Psl_Sequence (Expr : PSL_Node) is
   begin
      PSL.Prints.HDL_Expr_Printer := Disp_PSL_HDL_Expr'Access;
      PSL.Prints.Print_Sequence (Expr);
   end Disp_Psl_Sequence;

   procedure Disp_Psl_Default_Clock (Stmt : Iir) is
   begin
      if Vhdl_Std < Vhdl_08 then
         Put ("--psl ");
      end if;
      Put ("default clock is ");
      Disp_Psl_Expression (Get_Psl_Boolean (Stmt));
      Put_Line (";");
   end Disp_Psl_Default_Clock;

   procedure Disp_Psl_Declaration (Stmt : Iir)
   is
      use PSL.Nodes;
      Decl : constant PSL_Node := Get_Psl_Declaration (Stmt);
   begin
      if Vhdl_Std < Vhdl_08 then
         Put ("--psl ");
      end if;
      case Get_Kind (Decl) is
         when N_Property_Declaration =>
            Put ("property ");
            Disp_Ident (Get_Identifier (Decl));
            Put (" is ");
            Disp_Psl_Expression (Get_Property (Decl));
            Put_Line (";");
         when N_Sequence_Declaration =>
            Put ("sequence ");
            Disp_Ident (Get_Identifier (Decl));
            Put (" is ");
            Disp_Psl_Sequence (Get_Sequence (Decl));
            Put_Line (";");
         when N_Endpoint_Declaration =>
            Put ("endpoint ");
            Disp_Ident (Get_Identifier (Decl));
            Put (" is ");
            Disp_Psl_Sequence (Get_Sequence (Decl));
            Put_Line (";");
            Disp_PSL_NFA (Get_PSL_NFA (Stmt));
         when others =>
            Error_Kind ("disp_psl_declaration", Decl);
      end case;
   end Disp_Psl_Declaration;

   procedure Disp_Declaration_Chain (Parent : Iir; Indent: Count)
   is
      Decl: Iir;
   begin
      Decl := Get_Declaration_Chain (Parent);
      while Decl /= Null_Iir loop
         Set_Col (Indent);
         case Get_Kind (Decl) is
            when Iir_Kind_Type_Declaration =>
               Disp_Type_Declaration (Decl);
            when Iir_Kind_Anonymous_Type_Declaration =>
               Disp_Anonymous_Type_Declaration (Decl);
            when Iir_Kind_Subtype_Declaration =>
               Disp_Subtype_Declaration (Decl);
            when Iir_Kind_Use_Clause =>
               Disp_Use_Clause (Decl);
            when Iir_Kind_Component_Declaration =>
               Disp_Component_Declaration (Decl);
            when Iir_Kind_File_Declaration
              | Iir_Kind_Signal_Declaration
              | Iir_Kind_Constant_Declaration
              | Iir_Kind_Variable_Declaration =>
               Disp_Object_Declaration (Decl);
               while Get_Has_Identifier_List (Decl) loop
                  Decl := Get_Chain (Decl);
               end loop;
            when Iir_Kind_Object_Alias_Declaration =>
               Disp_Object_Alias_Declaration (Decl);
            when Iir_Kind_Terminal_Declaration =>
               Disp_Terminal_Declaration (Decl);
            when Iir_Kinds_Quantity_Declaration =>
               Disp_Quantity_Declaration (Decl);
            when Iir_Kind_Nature_Declaration =>
               Disp_Nature_Declaration (Decl);
            when Iir_Kind_Non_Object_Alias_Declaration =>
               Disp_Non_Object_Alias_Declaration (Decl);
            when Iir_Kind_Function_Declaration
              | Iir_Kind_Procedure_Declaration =>
               Disp_Subprogram_Declaration (Decl);
               if not Get_Has_Body (Decl) then
                  Put_Line (";");
               end if;
            when Iir_Kind_Function_Body
              | Iir_Kind_Procedure_Body =>
               --  The declaration was just displayed.
               Put_Line ("is");
               Set_Col (Indent);
               Disp_Subprogram_Body (Decl);
            when Iir_Kind_Protected_Type_Body =>
               Disp_Protected_Type_Body (Decl, Indent);
            when Iir_Kind_Configuration_Specification =>
               Disp_Configuration_Specification (Decl);
            when Iir_Kind_Disconnection_Specification =>
               Disp_Disconnection_Specification (Decl);
            when Iir_Kind_Attribute_Declaration =>
               Disp_Attribute_Declaration (Decl);
            when Iir_Kind_Attribute_Specification =>
               Disp_Attribute_Specification (Decl);
            when Iir_Kind_Signal_Attribute_Declaration =>
               null;
            when Iir_Kind_Group_Template_Declaration =>
               Disp_Group_Template_Declaration (Decl);
            when Iir_Kind_Group_Declaration =>
               Disp_Group_Declaration (Decl);
            when Iir_Kind_Package_Declaration =>
               Disp_Package_Declaration (Decl);
            when Iir_Kind_Package_Body =>
               Disp_Package_Body (Decl);
            when Iir_Kind_Package_Instantiation_Declaration =>
               Disp_Package_Instantiation_Declaration (Decl);
            when Iir_Kind_Psl_Default_Clock =>
               Disp_Psl_Default_Clock (Decl);
            when others =>
               Error_Kind ("disp_declaration_chain", Decl);
         end case;
         Decl := Get_Chain (Decl);
      end loop;
   end Disp_Declaration_Chain;

   procedure Disp_Waveform (Chain : Iir_Waveform_Element)
   is
      We: Iir_Waveform_Element;
      Val : Iir;
   begin
      if Chain = Null_Iir then
         Put ("null after {disconnection_time}");
         return;
      elsif Get_Kind (Chain) = Iir_Kind_Unaffected_Waveform then
         Put ("unaffected");
         return;
      end if;
      We := Chain;
      while We /= Null_Iir loop
         if We /= Chain then
            Put (", ");
         end if;
         Val := Get_We_Value (We);
         Disp_Expression (Val);
         if Get_Time (We) /= Null_Iir then
            Put (" after ");
            Disp_Expression (Get_Time (We));
         end if;
         We := Get_Chain (We);
      end loop;
   end Disp_Waveform;

   procedure Disp_Delay_Mechanism (Stmt: Iir) is
      Expr: Iir;
   begin
      case Get_Delay_Mechanism (Stmt) is
         when Iir_Transport_Delay =>
            Put ("transport ");
         when Iir_Inertial_Delay =>
            Expr := Get_Reject_Time_Expression (Stmt);
            if Expr /= Null_Iir then
               Put ("reject ");
               Disp_Expression (Expr);
               Put (" inertial ");
            end if;
      end case;
   end Disp_Delay_Mechanism;

   procedure Disp_Simple_Signal_Assignment (Stmt: Iir) is
   begin
      Disp_Expression (Get_Target (Stmt));
      Put (" <= ");
      Disp_Delay_Mechanism (Stmt);
      Disp_Waveform (Get_Waveform_Chain (Stmt));
      Put_Line (";");
   end Disp_Simple_Signal_Assignment;

   procedure Disp_Conditional_Waveform (Chain : Iir)
   is
      Cond_Wf : Iir;
      Indent : Count;
      Expr : Iir;
   begin
      Indent := Col;
      Set_Col (Indent);
      Cond_Wf := Chain;
      while Cond_Wf /= Null_Iir loop
         Disp_Waveform (Get_Waveform_Chain (Cond_Wf));
         Expr := Get_Condition (Cond_Wf);
         if Expr /= Null_Iir then
            Put (" when ");
            Disp_Expression (Expr);
            Put_Line (" else");
            Set_Col (Indent);
         end if;
         Cond_Wf := Get_Chain (Cond_Wf);
      end loop;
   end Disp_Conditional_Waveform;

   procedure Disp_Conditional_Signal_Assignment (Stmt: Iir) is
   begin
      Disp_Expression (Get_Target (Stmt));
      Put (" <= ");
      Disp_Delay_Mechanism (Stmt);
      Disp_Conditional_Waveform (Get_Conditional_Waveform_Chain (Stmt));
      Put_Line (";");
   end Disp_Conditional_Signal_Assignment;

   procedure Disp_Selected_Waveforms (Stmt : Iir; Indent : Count)
   is
      Assoc_Chain : constant Iir := Get_Selected_Waveform_Chain (Stmt);
      Assoc: Iir;
   begin
      Assoc := Assoc_Chain;
      while Assoc /= Null_Iir loop
         if Assoc /= Assoc_Chain then
            Put_Line (",");
         end if;
         Set_Col (Indent + Indentation);
         Disp_Waveform (Get_Associated_Chain (Assoc));
         Put (" when ");
         Disp_Choice (Assoc);
      end loop;
      Put_Line (";");
   end Disp_Selected_Waveforms;

   procedure Disp_Selected_Waveform_Assignment (Stmt: Iir; Indent : Count) is
   begin
      Put ("with ");
      Disp_Expression (Get_Expression (Stmt));
      Put (" select ");
      Disp_Expression (Get_Target (Stmt));
      Put (" <= ");
      Disp_Delay_Mechanism (Stmt);
      Disp_Selected_Waveforms (Stmt, Indent);
   end Disp_Selected_Waveform_Assignment;

   procedure Disp_Variable_Assignment (Stmt: Iir) is
   begin
      Disp_Expression (Get_Target (Stmt));
      Put (" := ");
      Disp_Expression (Get_Expression (Stmt));
      Put_Line (";");
   end Disp_Variable_Assignment;

   procedure Disp_Conditional_Expression (Exprs : Iir)
   is
      Expr : Iir;
      Cond : Iir;
   begin
      Expr := Exprs;
      loop
         Disp_Expression (Get_Expression (Expr));
         Cond := Get_Condition (Expr);
         if Cond /= Null_Iir then
            Put (" when ");
            Disp_Expression (Cond);
         end if;
         Expr := Get_Chain (Expr);
         exit when Expr = Null_Iir;
         Put (" else ");
      end loop;
   end Disp_Conditional_Expression;

   procedure Disp_Conditional_Variable_Assignment (Stmt: Iir) is
   begin
      Disp_Expression (Get_Target (Stmt));
      Put (" := ");
      Disp_Conditional_Expression (Get_Conditional_Expression (Stmt));
      Put_Line (";");
   end Disp_Conditional_Variable_Assignment;

   procedure Disp_Label (Stmt : Iir)
   is
      Label: constant Name_Id := Get_Label (Stmt);
   begin
      if Label /= Null_Identifier then
         Disp_Ident (Label);
         Put (": ");
      end if;
   end Disp_Label;

   procedure Disp_Postponed (Stmt : Iir) is
   begin
      if Get_Postponed_Flag (Stmt) then
         Put ("postponed ");
      end if;
   end Disp_Postponed;

   procedure Disp_Concurrent_Simple_Signal_Assignment (Stmt: Iir)
   is
      Indent: Count;
   begin
      Disp_Label (Stmt);
      Disp_Postponed (Stmt);
      Disp_Expression (Get_Target (Stmt));
      Put (" <= ");
      if Get_Guard (Stmt) /= Null_Iir then
         Put ("guarded ");
      end if;
      Disp_Delay_Mechanism (Stmt);
      Indent := Col;
      Set_Col (Indent);
      Disp_Waveform (Get_Waveform_Chain (Stmt));

      Put_Line (";");
   end Disp_Concurrent_Simple_Signal_Assignment;

   procedure Disp_Concurrent_Selected_Signal_Assignment (Stmt: Iir)
   is
      Indent: constant Count := Col;
   begin
      Set_Col (Indent);
      Disp_Label (Stmt);
      Disp_Postponed (Stmt);
      Put ("with ");
      Disp_Expression (Get_Expression (Stmt));
      Put (" select ");
      Disp_Expression (Get_Target (Stmt));
      Put (" <= ");
      if Get_Guard (Stmt) /= Null_Iir then
         Put ("guarded ");
      end if;
      Disp_Delay_Mechanism (Stmt);
      Disp_Selected_Waveforms (Stmt, Indent);
   end Disp_Concurrent_Selected_Signal_Assignment;

   procedure Disp_Concurrent_Conditional_Signal_Assignment (Stmt: Iir) is
   begin
      Disp_Label (Stmt);
      Disp_Postponed (Stmt);
      Disp_Expression (Get_Target (Stmt));
      Put (" <= ");
      if Get_Guard (Stmt) /= Null_Iir then
         Put ("guarded ");
      end if;
      Disp_Delay_Mechanism (Stmt);
      Disp_Conditional_Waveform (Get_Conditional_Waveform_Chain (Stmt));
      Put_Line (";");
   end Disp_Concurrent_Conditional_Signal_Assignment;

   procedure Disp_Assertion_Statement (Stmt: Iir)
   is
      Start: constant Count := Col;
      Expr: Iir;
   begin
      if Get_Kind (Stmt) = Iir_Kind_Concurrent_Assertion_Statement then
         Disp_Label (Stmt);
         Disp_Postponed (Stmt);
      end if;
      Put ("assert ");
      Disp_Expression (Get_Assertion_Condition (Stmt));
      Expr := Get_Report_Expression (Stmt);
      if Expr /= Null_Iir then
         Set_Col (Start + Indentation);
         Put ("report ");
         Disp_Expression (Expr);
      end if;
      Expr := Get_Severity_Expression (Stmt);
      if Expr /= Null_Iir then
         Set_Col (Start + Indentation);
         Put ("severity ");
         Disp_Expression (Expr);
      end if;
      Put_Line (";");
   end Disp_Assertion_Statement;

   procedure Disp_Report_Statement (Stmt: Iir)
   is
      Start: Count;
      Expr: Iir;
   begin
      Start := Col;
      Put ("report ");
      Expr := Get_Report_Expression (Stmt);
      Disp_Expression (Expr);
      Expr := Get_Severity_Expression (Stmt);
      if Expr /= Null_Iir then
         Set_Col (Start + Indentation);
         Put ("severity ");
         Disp_Expression (Expr);
      end if;
      Put_Line (";");
   end Disp_Report_Statement;

   procedure Disp_Dyadic_Operator (Expr: Iir) is
   begin
      if Flag_Parenthesis then
         Put ("(");
      end if;
      Disp_Expression (Get_Left (Expr));
      Put (' ' & Name_Table.Image (Iirs_Utils.Get_Operator_Name (Expr)) & ' ');
      Disp_Expression (Get_Right (Expr));
      if Flag_Parenthesis then
         Put (")");
      end if;
   end Disp_Dyadic_Operator;

   procedure Disp_Monadic_Operator (Expr: Iir) is
   begin
      if Get_Kind (Expr) = Iir_Kind_Implicit_Condition_Operator then
         Disp_Expression (Get_Operand (Expr));
         return;
      end if;

      Put (Name_Table.Image (Iirs_Utils.Get_Operator_Name (Expr)));
      Put (' ');
      if Flag_Parenthesis then
         Put ('(');
      end if;
      Disp_Expression (Get_Operand (Expr));
      if Flag_Parenthesis then
         Put (')');
      end if;
   end Disp_Monadic_Operator;

   procedure Disp_Case_Statement (Stmt: Iir_Case_Statement)
   is
      Indent: Count;
      Assoc: Iir;
      Sel_Stmt : Iir;
   begin
      Indent := Col;
      Put ("case ");
      Disp_Expression (Get_Expression (Stmt));
      Put_Line (" is");
      Assoc := Get_Case_Statement_Alternative_Chain (Stmt);
      while Assoc /= Null_Iir loop
         Set_Col (Indent + Indentation);
         Put ("when ");
         Sel_Stmt := Get_Associated_Chain (Assoc);
         Disp_Choice (Assoc);
         Put_Line (" =>");
         Set_Col (Indent + 2 * Indentation);
         Disp_Sequential_Statements (Sel_Stmt);
      end loop;
      Set_Col (Indent);
      Disp_End_Label (Stmt, "case");
   end Disp_Case_Statement;

   procedure Disp_Wait_Statement (Stmt: Iir_Wait_Statement) is
      List: Iir_List;
      Expr: Iir;
   begin
      Put ("wait");
      List := Get_Sensitivity_List (Stmt);
      if List /= Null_Iir_List then
         Put (" on ");
         Disp_Designator_List (List);
      end if;
      Expr := Get_Condition_Clause (Stmt);
      if Expr /= Null_Iir then
         Put (" until ");
         Disp_Expression (Expr);
      end if;
      Expr := Get_Timeout_Clause (Stmt);
      if Expr /= Null_Iir then
         Put (" for ");
         Disp_Expression (Expr);
      end if;
      Put_Line (";");
   end Disp_Wait_Statement;

   procedure Disp_If_Statement (Stmt: Iir_If_Statement) is
      Clause: Iir;
      Expr: Iir;
      Start: Count;
   begin
      Start := Col;
      Put ("if ");
      Clause := Stmt;
      Disp_Expression (Get_Condition (Clause));
      Put_Line (" then");
      while Clause /= Null_Iir loop
         Set_Col (Start + Indentation);
         Disp_Sequential_Statements (Get_Sequential_Statement_Chain (Clause));
         Clause := Get_Else_Clause (Clause);
         exit when Clause = Null_Iir;
         Expr := Get_Condition (Clause);
         Set_Col (Start);
         if Expr /= Null_Iir then
            Put ("elsif ");
            Disp_Expression (Expr);
            Put_Line (" then");
         else
            Put_Line ("else");
         end if;
      end loop;
      Set_Col (Start);
      Disp_End_Label (Stmt, "if");
   end Disp_If_Statement;

   procedure Disp_Parameter_Specification
     (Iterator : Iir_Iterator_Declaration) is
   begin
      Disp_Identifier (Iterator);
      Put (" in ");
      Disp_Discrete_Range (Get_Subtype_Indication (Iterator));
   end Disp_Parameter_Specification;

   procedure Disp_Method_Object (Call : Iir)
   is
      Obj : Iir;
   begin
      Obj := Get_Method_Object (Call);
      if Obj /= Null_Iir then
         Disp_Name (Obj);
         Put ('.');
      end if;
   end Disp_Method_Object;

   procedure Disp_Procedure_Call (Call : Iir) is
   begin
      if True then
         Disp_Name (Get_Prefix (Call));
      else
         Disp_Method_Object (Call);
         Disp_Identifier (Get_Implementation (Call));
         Put (' ');
      end if;
      Disp_Association_Chain (Get_Parameter_Association_Chain (Call));
      Put_Line (";");
   end Disp_Procedure_Call;

   procedure Disp_Sequential_Statements (First : Iir)
   is
      Start: constant Count := Col;
      Stmt: Iir;
   begin
      Stmt := First;
      while Stmt /= Null_Iir loop
         Set_Col (Start);
         Disp_Label (Stmt);
         case Iir_Kinds_Sequential_Statement (Get_Kind (Stmt)) is
            when Iir_Kind_Null_Statement =>
               Put_Line ("null;");
            when Iir_Kind_If_Statement =>
               Disp_If_Statement (Stmt);
            when Iir_Kind_For_Loop_Statement =>
               Put ("for ");
               Disp_Parameter_Specification
                 (Get_Parameter_Specification (Stmt));
               Put_Line (" loop");
               Set_Col (Start + Indentation);
               Disp_Sequential_Statements
                 (Get_Sequential_Statement_Chain (Stmt));
               Set_Col (Start);
               Disp_End_Label (Stmt, "loop");
            when Iir_Kind_While_Loop_Statement =>
               if Get_Condition (Stmt) /= Null_Iir then
                  Put ("while ");
                  Disp_Expression (Get_Condition (Stmt));
                  Put (" ");
               end if;
               Put_Line ("loop");
               Set_Col (Start + Indentation);
               Disp_Sequential_Statements
                 (Get_Sequential_Statement_Chain (Stmt));
               Set_Col (Start);
               Disp_End_Label (Stmt, "loop");
            when Iir_Kind_Simple_Signal_Assignment_Statement =>
               Disp_Simple_Signal_Assignment (Stmt);
            when Iir_Kind_Conditional_Signal_Assignment_Statement =>
               Disp_Conditional_Signal_Assignment (Stmt);
            when Iir_Kind_Selected_Waveform_Assignment_Statement =>
               Disp_Selected_Waveform_Assignment (Stmt, Start);
            when Iir_Kind_Variable_Assignment_Statement =>
               Disp_Variable_Assignment (Stmt);
            when Iir_Kind_Conditional_Variable_Assignment_Statement =>
               Disp_Conditional_Variable_Assignment (Stmt);
            when Iir_Kind_Assertion_Statement =>
               Disp_Assertion_Statement (Stmt);
            when Iir_Kind_Report_Statement =>
               Disp_Report_Statement (Stmt);
            when Iir_Kind_Return_Statement =>
               if Get_Expression (Stmt) /= Null_Iir then
                  Put ("return ");
                  Disp_Expression (Get_Expression (Stmt));
                  Put_Line (";");
               else
                  Put_Line ("return;");
               end if;
            when Iir_Kind_Case_Statement =>
               Disp_Case_Statement (Stmt);
            when Iir_Kind_Wait_Statement =>
               Disp_Wait_Statement (Stmt);
            when Iir_Kind_Procedure_Call_Statement =>
               Disp_Procedure_Call (Get_Procedure_Call (Stmt));
            when Iir_Kind_Exit_Statement
              | Iir_Kind_Next_Statement =>
               declare
                  Label : constant Iir := Get_Loop_Label (Stmt);
                  Cond : constant Iir := Get_Condition (Stmt);
               begin
                  if Get_Kind (Stmt) = Iir_Kind_Exit_Statement then
                     Put ("exit");
                  else
                     Put ("next");
                  end if;
                  if Label /= Null_Iir then
                     Put (" ");
                     Disp_Name (Label);
                  end if;
                  if Cond /= Null_Iir then
                     Put (" when ");
                     Disp_Expression (Cond);
                  end if;
                  Put_Line (";");
               end;
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;
   end Disp_Sequential_Statements;

   procedure Disp_Process_Statement (Process: Iir)
   is
      Start: constant Count := Col;
   begin
      Disp_Label (Process);
      Disp_Postponed (Process);

      Put ("process ");
      if Get_Kind (Process) = Iir_Kind_Sensitized_Process_Statement then
         Put ("(");
         Disp_Designator_List (Get_Sensitivity_List (Process));
         Put (")");
      end if;
      if Get_Has_Is (Process) then
         Put (" is");
      end if;
      New_Line;
      Disp_Declaration_Chain (Process, Start + Indentation);
      Set_Col (Start);
      Put_Line ("begin");
      Set_Col (Start + Indentation);
      Disp_Sequential_Statements (Get_Sequential_Statement_Chain (Process));
      Set_Col (Start);
      Put ("end");
      if Get_End_Has_Postponed (Process) then
         Put (" postponed");
      end if;
      Disp_After_End (Process, "process");
   end Disp_Process_Statement;

   procedure Disp_Conversion (Conv : Iir) is
   begin
      case Get_Kind (Conv) is
         when Iir_Kind_Function_Call =>
            Disp_Function_Name (Get_Implementation (Conv));
         when Iir_Kind_Type_Conversion =>
            Disp_Name_Of (Get_Type_Mark (Conv));
         when others =>
            Error_Kind ("disp_conversion", Conv);
      end case;
   end Disp_Conversion;

   procedure Disp_Association_Chain (Chain : Iir)
   is
      El: Iir;
      Formal: Iir;
      Need_Comma : Boolean;
      Conv : Iir;
   begin
      if Chain = Null_Iir then
         return;
      end if;
      Put ("(");
      Need_Comma := False;

      El := Chain;
      while El /= Null_Iir loop
         if Get_Kind (El) /= Iir_Kind_Association_Element_By_Individual then
            if Need_Comma then
               Put (", ");
            end if;

            --  Formal part.
            if Get_Kind (El) = Iir_Kind_Association_Element_By_Expression then
               Conv := Get_Formal_Conversion (El);
               if Conv /= Null_Iir then
                  Disp_Conversion (Conv);
                  Put (" (");
               end if;
            else
               Conv := Null_Iir;
            end if;
            Formal := Get_Formal (El);
            if Formal /= Null_Iir then
               case Get_Kind (El) is
                  when Iir_Kind_Association_Element_Package
                    | Iir_Kind_Association_Element_Type
                    | Iir_Kind_Association_Element_Subprogram =>
                     Disp_Name (Formal);
                  when Iir_Kind_Association_Element_By_Expression
                    | Iir_Kind_Association_Element_By_Individual
                    | Iir_Kind_Association_Element_Open =>
                     Disp_Expression (Formal);
                  when others =>
                     raise Internal_Error;
               end case;
               if Conv /= Null_Iir then
                  Put (")");
               end if;
               Put (" => ");
            end if;

            case Get_Kind (El) is
               when Iir_Kind_Association_Element_Open =>
                  Put ("open");
               when Iir_Kind_Association_Element_Package
                 | Iir_Kind_Association_Element_Type
                 | Iir_Kind_Association_Element_Subprogram =>
                  Disp_Name (Get_Actual (El));
               when others =>
                  Conv := Get_Actual_Conversion (El);
                  if Conv /= Null_Iir then
                     Disp_Conversion (Conv);
                     Put (" (");
                  end if;
                  Disp_Expression (Get_Actual (El));
                  if Conv /= Null_Iir then
                     Put (")");
                  end if;
            end case;
            Need_Comma := True;
         end if;
         El := Get_Chain (El);
      end loop;
      Put (")");
   end Disp_Association_Chain;

   procedure Disp_Generic_Map_Aspect (Parent : Iir) is
   begin
      Put ("generic map ");
      Disp_Association_Chain (Get_Generic_Map_Aspect_Chain (Parent));
   end Disp_Generic_Map_Aspect;

   procedure Disp_Port_Map_Aspect (Parent : Iir) is
   begin
      Put ("port map ");
      Disp_Association_Chain (Get_Port_Map_Aspect_Chain (Parent));
   end Disp_Port_Map_Aspect;

   procedure Disp_Entity_Aspect (Aspect : Iir) is
      Arch : Iir;
   begin
      case Get_Kind (Aspect) is
         when Iir_Kind_Entity_Aspect_Entity =>
            Put ("entity ");
            Disp_Name (Get_Entity_Name (Aspect));
            Arch := Get_Architecture (Aspect);
            if Arch /= Null_Iir then
               Put (" (");
               Disp_Name_Of (Arch);
               Put (")");
            end if;
         when Iir_Kind_Entity_Aspect_Configuration =>
            Put ("configuration ");
            Disp_Name (Get_Configuration_Name (Aspect));
         when Iir_Kind_Entity_Aspect_Open =>
            Put ("open");
         when others =>
            Error_Kind ("disp_entity_aspect", Aspect);
      end case;
   end Disp_Entity_Aspect;

   procedure Disp_Component_Instantiation_Statement
     (Stmt: Iir_Component_Instantiation_Statement)
   is
      Component: constant Iir := Get_Instantiated_Unit (Stmt);
      Alist: Iir;
   begin
      Disp_Label (Stmt);
      if Get_Kind (Component) in Iir_Kinds_Denoting_Name then
         if Get_Has_Component (Stmt) then
            Put ("component");
            Put (" ");
         end if;
         Disp_Name (Component);
      else
         Disp_Entity_Aspect (Component);
      end if;
      Alist := Get_Generic_Map_Aspect_Chain (Stmt);
      if Alist /= Null_Iir then
         Put (" ");
         Disp_Generic_Map_Aspect (Stmt);
      end if;
      Alist := Get_Port_Map_Aspect_Chain (Stmt);
      if Alist /= Null_Iir then
         Put (" ");
         Disp_Port_Map_Aspect (Stmt);
      end if;
      Put (";");
   end Disp_Component_Instantiation_Statement;

   procedure Disp_Function_Call (Expr: Iir_Function_Call) is
   begin
      if True then
         Disp_Name (Get_Prefix (Expr));
      else
         Disp_Method_Object (Expr);
         Disp_Function_Name (Get_Implementation (Expr));
      end if;
      Disp_Association_Chain (Get_Parameter_Association_Chain (Expr));
   end Disp_Function_Call;

   procedure Disp_Indexed_Name (Indexed: Iir)
   is
      List : Iir_Flist;
      El: Iir;
   begin
      Disp_Expression (Get_Prefix (Indexed));
      Put (" (");
      List := Get_Index_List (Indexed);
      for I in Flist_First .. Flist_Last (List) loop
         El := Get_Nth_Element (List, I);
         if I /= 0 then
            Put (", ");
         end if;
         Disp_Expression (El);
      end loop;
      Put (")");
   end Disp_Indexed_Name;

   procedure Disp_A_Choice (Choice : Iir) is
   begin
      case Iir_Kinds_Choice (Get_Kind (Choice)) is
         when Iir_Kind_Choice_By_Others =>
            Put ("others");
         when Iir_Kind_Choice_By_None =>
            null;
         when Iir_Kind_Choice_By_Expression =>
            Disp_Expression (Get_Choice_Expression (Choice));
         when Iir_Kind_Choice_By_Range =>
            Disp_Range (Get_Choice_Range (Choice));
         when Iir_Kind_Choice_By_Name =>
            Disp_Name_Of (Get_Choice_Name (Choice));
      end case;
   end Disp_A_Choice;

   procedure Disp_Choice (Choice: in out Iir) is
   begin
      loop
         Disp_A_Choice (Choice);
         Choice := Get_Chain (Choice);
         exit when Choice = Null_Iir;
         exit when Get_Same_Alternative_Flag (Choice) = False;
         --exit when Choice = Null_Iir;
         Put (" | ");
      end loop;
   end Disp_Choice;

   --  Build an array of lexical appareance of choices in CHAIN.
   --  (They have been re-ordered during analysis).
   procedure Build_Choice_Order (Chain : Iir; Arr : out Iir_Array_Acc)
   is
      Count : Natural;
      Assoc : Iir;
   begin
      Assoc := Chain;
      Count := 0;
      while Assoc /= Null_Iir loop
         Count := Count + 1;
         Assoc := Get_Chain (Assoc);
      end loop;
      Arr := new Iir_Array (0 .. Count - 1);

      Assoc := Chain;
      while Assoc /= Null_Iir loop
         Arr (Natural (Get_Choice_Position (Assoc))) := Assoc;
         Assoc := Get_Chain (Assoc);
      end loop;
   end Build_Choice_Order;

   --  EL_TYPE is Null_Iir for record aggregates.
   procedure Disp_Aggregate_1
     (Aggr: Iir_Aggregate; Index : Positive; El_Type : Iir)
   is
      Indent : Count;
      Assoc : Iir;
      Expr : Iir;
      Prev_Expr : Iir;
      Choices : Iir_Array_Acc;
   begin
      Indent := Col + 1;
      if Indent > Line_Length - 10 then
         Indent := 2 * Indentation;
      end if;
      Put ("(");
      Assoc := Get_Association_Choices_Chain (Aggr);
      Build_Choice_Order (Assoc, Choices);
      Prev_Expr := Null_Iir;
      for I in Choices'Range loop
         Assoc := Choices (I);
         Expr := Get_Associated_Expr (Assoc);
         pragma Assert (Expr /= Null_Iir);
         if Expr = Prev_Expr then
            Put (" | ");
         elsif I /= Choices'First then
            Put (", ");
         end if;
         Disp_A_Choice (Assoc);
         if I = Choices'Last
           or else Expr /= Get_Associated_Expr (Choices (I + 1))
         then
            if Get_Kind (Assoc) /= Iir_Kind_Choice_By_None then
               Put (" => ");
            end if;

            if Index > 1 then
               Set_Col (Indent);
               if Get_Kind (Expr) = Iir_Kind_String_Literal8 then
                  Disp_String_Literal (Expr, El_Type);
               else
                  Disp_Aggregate_1 (Expr, Index - 1, El_Type);
               end if;
            else
               if Get_Kind (Expr) = Iir_Kind_Aggregate then
                  Set_Col (Indent);
               end if;
               Disp_Expression (Expr);
            end if;
         end if;
         Prev_Expr := Expr;
      end loop;
      Put (")");

      Free (Choices);
   end Disp_Aggregate_1;

   procedure Disp_Aggregate (Aggr: Iir_Aggregate)
   is
      Aggr_Type : constant Iir := Get_Type (Aggr);
      Base_Type : Iir;
   begin
      if Aggr_Type /= Null_Iir
        and then Get_Kind (Aggr_Type) in Iir_Kinds_Array_Type_Definition
      then
         Base_Type := Get_Base_Type (Aggr_Type);
         Disp_Aggregate_1
           (Aggr, Get_Nbr_Elements (Get_Index_Subtype_List (Base_Type)),
            Get_Element_Subtype (Base_Type));
      else
         Disp_Aggregate_1 (Aggr, 1, Null_Iir);
      end if;
   end Disp_Aggregate;

   procedure Disp_Simple_Aggregate (Aggr: Iir_Simple_Aggregate)
   is
      List : constant Iir_Flist := Get_Simple_Aggregate_List (Aggr);
      El : Iir;
      First : Boolean := True;
   begin
      Put ("(");
      for I in Flist_First .. Flist_Last (List) loop
         El := Get_Nth_Element (List, I);
         if First then
            First := False;
         else
            Put (", ");
         end if;
         Disp_Expression (El);
      end loop;
      Put (")");
   end Disp_Simple_Aggregate;

   procedure Disp_Parametered_Attribute (Name : String; Expr : Iir)
   is
      Param : Iir;
      Pfx : Iir;
   begin
      Pfx := Get_Prefix (Expr);
      case Get_Kind (Pfx) is
         when Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration =>
            Disp_Name_Of (Pfx);
         when others =>
            Disp_Expression (Pfx);
      end case;
      Put ("'");
      Put (Name);
      Param := Get_Parameter (Expr);
      if Param /= Null_Iir
        and then Param /= Std_Package.Universal_Integer_One
      then
         Put (" (");
         Disp_Expression (Param);
         Put (")");
      end if;
   end Disp_Parametered_Attribute;

   procedure Disp_Parametered_Type_Attribute (Name : String; Expr : Iir) is
   begin
      Disp_Name (Get_Prefix (Expr));
      Put ("'");
      Put (Name);
      Put (" (");
      Disp_Expression (Get_Parameter (Expr));
      Put (")");
   end Disp_Parametered_Type_Attribute;

   procedure Disp_String_Literal (Str : Iir; El_Type : Iir)
   is
      Str_Id : constant String8_Id := Get_String8_Id (Str);
      Len : constant Nat32 := Get_String_Length (Str);
      Literal_List : constant Iir_Flist :=
        Get_Enumeration_Literal_List (Get_Base_Type (El_Type));
      Pos : Nat8;
      Lit : Iir;
      Id : Name_Id;
      C : Character;
   begin
      if Get_Bit_String_Base (Str) /= Base_None then
         if Get_Has_Length (Str) then
            Disp_Int32 (Iir_Int32 (Get_String_Length (Str)));
         end if;
         Put ("b");
      end if;

      Put ("""");

      for I in 1 .. Len loop
         Pos := Str_Table.Element_String8 (Str_Id, I);
         Lit := Get_Nth_Element (Literal_List, Natural (Pos));
         Id := Get_Identifier (Lit);
         pragma Assert (Name_Table.Is_Character (Id));
         C := Name_Table.Get_Character (Id);
         if C = '"' then
            Put ('"');
         end if;
         Put (C);
      end loop;

      Put ("""");
   end Disp_String_Literal;

   procedure Disp_Expression (Expr: Iir)
   is
      Orig : Iir;
   begin
      case Get_Kind (Expr) is
         when Iir_Kind_Integer_Literal =>
            Orig := Get_Literal_Origin (Expr);
            if Dump_Origin_Flag and then Orig /= Null_Iir then
               Disp_Expression (Orig);
            else
               Disp_Int64 (Get_Value (Expr));
            end if;
         when Iir_Kind_Floating_Point_Literal =>
            Orig := Get_Literal_Origin (Expr);
            if Dump_Origin_Flag and then Orig /= Null_Iir then
               Disp_Expression (Orig);
            else
               Disp_Fp64 (Get_Fp_Value (Expr));
            end if;
         when Iir_Kind_String_Literal8 =>
            Orig := Get_Literal_Origin (Expr);
            if Dump_Origin_Flag and then Orig /= Null_Iir then
               Disp_Expression (Orig);
            else
               Disp_String_Literal
                 (Expr, Get_Element_Subtype (Get_Type (Expr)));
               if Flag_Disp_String_Literal_Type or Flags.List_Verbose then
                  Put ("[type: ");
                  Disp_Type (Get_Type (Expr));
                  Put ("]");
               end if;
            end if;
         when Iir_Kind_Physical_Fp_Literal
           | Iir_Kind_Physical_Int_Literal =>
            Orig := Get_Literal_Origin (Expr);
            if Dump_Origin_Flag and then Orig /= Null_Iir then
               Disp_Expression (Orig);
            else
               Disp_Physical_Literal (Expr);
            end if;
         when Iir_Kind_Unit_Declaration =>
            Disp_Name_Of (Expr);
         when Iir_Kind_Character_Literal =>
            Disp_Identifier (Expr);
         when Iir_Kind_Enumeration_Literal =>
            Orig := Get_Literal_Origin (Expr);
            if Dump_Origin_Flag and then Orig /= Null_Iir then
               Disp_Expression (Orig);
            else
               Disp_Name_Of (Expr);
            end if;
         when Iir_Kind_Overflow_Literal =>
            Orig := Get_Literal_Origin (Expr);
            if Dump_Origin_Flag and then Orig /= Null_Iir then
               Disp_Expression (Orig);
            else
               Put ("*OVERFLOW*");
            end if;

         when Iir_Kind_Object_Alias_Declaration =>
            Disp_Name_Of (Expr);
         when Iir_Kind_Aggregate =>
            Disp_Aggregate (Expr);
         when Iir_Kind_Null_Literal =>
            Put ("null");
         when Iir_Kind_Simple_Aggregate =>
            Orig := Get_Literal_Origin (Expr);
            if Dump_Origin_Flag and then Orig /= Null_Iir then
               Disp_Expression (Orig);
            else
               Disp_Simple_Aggregate (Expr);
            end if;

         when Iir_Kind_Attribute_Value =>
            Disp_Attribute_Value (Expr);
         when Iir_Kind_Attribute_Name =>
            Disp_Attribute_Name (Expr);

         when Iir_Kind_Element_Declaration =>
            Disp_Name_Of (Expr);

         when Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Iterator_Declaration =>
            Disp_Name_Of (Expr);
            return;
         when Iir_Kind_Reference_Name =>
            declare
               Name : constant Iir := Get_Referenced_Name (Expr);
            begin
               if Is_Valid (Name) then
                  Disp_Name (Name);
               else
                  Disp_Expression (Get_Named_Entity (Expr));
               end if;
            end;

         when Iir_Kinds_Dyadic_Operator =>
            Disp_Dyadic_Operator (Expr);
         when Iir_Kinds_Monadic_Operator =>
            Disp_Monadic_Operator (Expr);
         when Iir_Kind_Function_Call =>
            Disp_Function_Call (Expr);
         when Iir_Kind_Parenthesis_Expression =>
            Put ("(");
            Disp_Expression (Get_Expression (Expr));
            Put (")");
         when Iir_Kind_Type_Conversion =>
            Disp_Name (Get_Type_Mark (Expr));
            Put (" (");
            Disp_Expression (Get_Expression (Expr));
            Put (")");
         when Iir_Kind_Qualified_Expression =>
            declare
               Qexpr : constant Iir := Get_Expression (Expr);
               Has_Paren : constant Boolean :=
                 Get_Kind (Qexpr) = Iir_Kind_Parenthesis_Expression
                 or else Get_Kind (Qexpr) = Iir_Kind_Aggregate;
            begin
               Disp_Name (Get_Type_Mark (Expr));
               Put ("'");
               if not Has_Paren then
                  Put ("(");
               end if;
               Disp_Expression (Qexpr);
               if not Has_Paren then
                  Put (")");
               end if;
            end;
         when Iir_Kind_Allocator_By_Expression =>
            Put ("new ");
            Disp_Expression (Get_Expression (Expr));
         when Iir_Kind_Allocator_By_Subtype =>
            Put ("new ");
            Disp_Subtype_Indication (Get_Subtype_Indication (Expr));

         when Iir_Kind_Indexed_Name =>
            Disp_Indexed_Name (Expr);
         when Iir_Kind_Slice_Name =>
            Disp_Expression (Get_Prefix (Expr));
            Put (" (");
            Disp_Range (Get_Suffix (Expr));
            Put (")");
         when Iir_Kind_Selected_Element =>
            Disp_Expression (Get_Prefix (Expr));
            Put (".");
            Disp_Name_Of (Get_Selected_Element (Expr));
         when Iir_Kind_Implicit_Dereference =>
            Disp_Expression (Get_Prefix (Expr));
         when Iir_Kind_Dereference =>
            Disp_Expression (Get_Prefix (Expr));
            Put (".all");

         when Iir_Kind_Left_Type_Attribute =>
            Disp_Name (Get_Prefix (Expr));
            Put ("'left");
         when Iir_Kind_Right_Type_Attribute =>
            Disp_Name (Get_Prefix (Expr));
            Put ("'right");
         when Iir_Kind_High_Type_Attribute =>
            Disp_Name (Get_Prefix (Expr));
            Put ("'high");
         when Iir_Kind_Low_Type_Attribute =>
            Disp_Name (Get_Prefix (Expr));
            Put ("'low");
         when Iir_Kind_Ascending_Type_Attribute =>
            Disp_Name (Get_Prefix (Expr));
            Put ("'ascending");

         when Iir_Kind_Stable_Attribute =>
            Disp_Parametered_Attribute ("stable", Expr);
         when Iir_Kind_Quiet_Attribute =>
            Disp_Parametered_Attribute ("quiet", Expr);
         when Iir_Kind_Delayed_Attribute =>
            Disp_Parametered_Attribute ("delayed", Expr);
         when Iir_Kind_Transaction_Attribute =>
            Disp_Expression (Get_Prefix (Expr));
            Put ("'transaction");
         when Iir_Kind_Event_Attribute =>
            Disp_Expression (Get_Prefix (Expr));
            Put ("'event");
         when Iir_Kind_Active_Attribute =>
            Disp_Expression (Get_Prefix (Expr));
            Put ("'active");
         when Iir_Kind_Driving_Attribute =>
            Disp_Expression (Get_Prefix (Expr));
            Put ("'driving");
         when Iir_Kind_Driving_Value_Attribute =>
            Disp_Expression (Get_Prefix (Expr));
            Put ("'driving_value");
         when Iir_Kind_Last_Value_Attribute =>
            Disp_Expression (Get_Prefix (Expr));
            Put ("'last_value");
         when Iir_Kind_Last_Active_Attribute =>
            Disp_Expression (Get_Prefix (Expr));
            Put ("'last_active");
         when Iir_Kind_Last_Event_Attribute =>
            Disp_Expression (Get_Prefix (Expr));
            Put ("'last_event");

         when Iir_Kind_Pos_Attribute =>
            Disp_Parametered_Type_Attribute ("pos", Expr);
         when Iir_Kind_Val_Attribute =>
            Disp_Parametered_Type_Attribute ("val", Expr);
         when Iir_Kind_Succ_Attribute =>
            Disp_Parametered_Type_Attribute ("succ", Expr);
         when Iir_Kind_Pred_Attribute =>
            Disp_Parametered_Type_Attribute ("pred", Expr);
         when Iir_Kind_Leftof_Attribute =>
            Disp_Parametered_Type_Attribute ("leftof", Expr);
         when Iir_Kind_Rightof_Attribute =>
            Disp_Parametered_Type_Attribute ("rightof", Expr);

         when Iir_Kind_Length_Array_Attribute =>
            Disp_Parametered_Attribute ("length", Expr);
         when Iir_Kind_Range_Array_Attribute =>
            Disp_Parametered_Attribute ("range", Expr);
         when Iir_Kind_Reverse_Range_Array_Attribute =>
            Disp_Parametered_Attribute ("reverse_range", Expr);
         when Iir_Kind_Left_Array_Attribute =>
            Disp_Parametered_Attribute ("left", Expr);
         when Iir_Kind_Right_Array_Attribute =>
            Disp_Parametered_Attribute ("right", Expr);
         when Iir_Kind_Low_Array_Attribute =>
            Disp_Parametered_Attribute ("low", Expr);
         when Iir_Kind_High_Array_Attribute =>
            Disp_Parametered_Attribute ("high", Expr);
         when Iir_Kind_Ascending_Array_Attribute =>
            Disp_Parametered_Attribute ("ascending", Expr);

         when Iir_Kind_Image_Attribute =>
            Disp_Parametered_Attribute ("image", Expr);
         when Iir_Kind_Value_Attribute =>
            Disp_Parametered_Attribute ("value", Expr);
         when Iir_Kind_Simple_Name_Attribute =>
            Disp_Name (Get_Prefix (Expr));
            Put ("'simple_name");
         when Iir_Kind_Instance_Name_Attribute =>
            Disp_Name (Get_Prefix (Expr));
            Put ("'instance_name");
         when Iir_Kind_Path_Name_Attribute =>
            Disp_Name (Get_Prefix (Expr));
            Put ("'path_name");

         when Iir_Kind_Selected_By_All_Name =>
            Disp_Expression (Get_Prefix (Expr));
         when Iir_Kind_Selected_Name =>
            Disp_Name (Expr);
         when Iir_Kind_Simple_Name =>
            Disp_Name (Expr);

         when Iir_Kinds_Type_And_Subtype_Definition =>
            Disp_Type (Expr);

         when Iir_Kind_Range_Expression =>
            Disp_Range (Expr);
         when Iir_Kind_Subtype_Declaration =>
            Disp_Name_Of (Expr);

         when others =>
            Error_Kind ("disp_expression", Expr);
      end case;
   end Disp_Expression;

   procedure Disp_Block_Header (Header : Iir_Block_Header; Indent: Count)
   is
      Chain : Iir;
   begin
      if Header = Null_Iir then
         return;
      end if;
      Chain := Get_Generic_Chain (Header);
      if Chain /= Null_Iir then
         Set_Col (Indent + Indentation);
         Disp_Generics (Header);
         Chain := Get_Generic_Map_Aspect_Chain (Header);
         if Chain /= Null_Iir then
            Set_Col (Indent + Indentation);
            Disp_Generic_Map_Aspect (Header);
            Put_Line (";");
         end if;
      end if;
      Chain := Get_Port_Chain (Header);
      if Chain /= Null_Iir then
         Set_Col (Indent + Indentation);
         Disp_Ports (Header);
         Chain := Get_Port_Map_Aspect_Chain (Header);
         if Chain /= Null_Iir then
            Set_Col (Indent + Indentation);
            Disp_Port_Map_Aspect (Header);
            Put_Line (";");
         end if;
      end if;
   end Disp_Block_Header;

   procedure Disp_Block_Statement (Block: Iir_Block_Statement)
   is
      Indent: Count;
      Sensitivity: Iir_List;
      Guard : Iir_Guard_Signal_Declaration;
   begin
      Indent := Col;
      Disp_Label (Block);
      Put ("block");
      Guard := Get_Guard_Decl (Block);
      if Guard /= Null_Iir then
         Put (" (");
         Disp_Expression (Get_Guard_Expression (Guard));
         Put_Line (")");
         Sensitivity := Get_Guard_Sensitivity_List (Guard);
         if Sensitivity /= Null_Iir_List then
            Set_Col (Indent + Indentation);
            Put ("-- guard sensitivity list ");
            Disp_Designator_List (Sensitivity);
         end if;
      else
         New_Line;
      end if;
      Disp_Block_Header (Get_Block_Header (Block),
                         Indent + Indentation);
      Disp_Declaration_Chain (Block, Indent + Indentation);
      Set_Col (Indent);
      Put_Line ("begin");
      Disp_Concurrent_Statement_Chain (Block, Indent + Indentation);
      Set_Col (Indent);
      Disp_End (Block, "block");
   end Disp_Block_Statement;

   procedure Disp_Generate_Statement_Body (Bod : Iir; Indent : Count) is
   begin
      Disp_Declaration_Chain (Bod, Indent);
      if Get_Has_Begin (Bod) then
         Set_Col (Indent);
         Put_Line ("begin");
      end if;
      Disp_Concurrent_Statement_Chain (Bod, Indent + Indentation);
      if Get_Has_End (Bod) then
         Set_Col (Indent);
         Put ("end");
         if Get_End_Has_Identifier (Bod) then
            Put (' ');
            Disp_Ident (Get_Alternative_Label (Bod));
         end if;
         Put (';');
         New_Line;
      end if;
   end Disp_Generate_Statement_Body;

   procedure Disp_For_Generate_Statement (Stmt : Iir)
   is
      Indent : constant Count := Col;
   begin
      Disp_Label (Stmt);
      Put ("for ");
      Disp_Parameter_Specification (Get_Parameter_Specification (Stmt));
      Put_Line (" generate");
      Disp_Generate_Statement_Body
        (Get_Generate_Statement_Body (Stmt), Indent);
      Set_Col (Indent);
      Disp_End (Stmt, "generate");
   end Disp_For_Generate_Statement;

   procedure Disp_If_Generate_Statement (Stmt : Iir)
   is
      Indent : constant Count := Col;
      Bod : Iir;
      Clause : Iir;
      Cond : Iir;
   begin
      Disp_Label (Stmt);
      Put ("if ");
      Cond := Get_Condition (Stmt);
      Clause := Stmt;
      loop
         Bod := Get_Generate_Statement_Body (Clause);
         if Get_Has_Label (Bod) then
            Disp_Ident (Get_Alternative_Label (Bod));
            Put (": ");
         end if;
         if Cond /= Null_Iir then
            Disp_Expression (Cond);
            Put (" ");
         end if;
         Put_Line ("generate");
         Disp_Generate_Statement_Body (Bod, Indent);

         Clause := Get_Generate_Else_Clause (Clause);
         exit when Clause = Null_Iir;

         Cond := Get_Condition (Clause);
         Set_Col (Indent);
         if Cond = Null_Iir then
            Put ("else ");
         else
            Put ("elsif ");
         end if;
      end loop;
      Set_Col (Indent);
      Disp_End (Stmt, "generate");
   end Disp_If_Generate_Statement;

   procedure Disp_Case_Generate_Statement (Stmt : Iir)
   is
      Indent : constant Count := Col;
      Bod : Iir;
      Assoc : Iir;
   begin
      Disp_Label (Stmt);
      Put ("case ");
      Disp_Expression (Get_Expression (Stmt));
      Put_Line (" generate");
      Assoc := Get_Case_Statement_Alternative_Chain (Stmt);
      while Assoc /= Null_Iir loop
         Set_Col (Indent + Indentation);
         Put ("when ");
         Bod := Get_Associated_Block (Assoc);
         if Get_Has_Label (Bod) then
            Disp_Ident (Get_Alternative_Label (Bod));
            Put (": ");
         end if;
         Disp_Choice (Assoc);
         Put (" ");
         Put_Line ("=>");
         Disp_Generate_Statement_Body (Bod, Indent + 2 * Indentation);
      end loop;
      Set_Col (Indent);
      Disp_End (Stmt, "generate");
   end Disp_Case_Generate_Statement;

   procedure Disp_PSL_NFA (N : PSL.Nodes.NFA)
   is
      use PSL.NFAs;

      procedure Disp_State (S : NFA_State) is
         Str : constant String := Int32'Image (Get_State_Label (S));
      begin
         Put (Str (2 .. Str'Last));
      end Disp_State;

      S : NFA_State;
      E : NFA_Edge;
   begin
      if N /= No_NFA then
         Put ("-- start: ");
         Disp_State (Get_Start_State (N));
         Put (", final: ");
         Disp_State (Get_Final_State (N));
         New_Line;

         S := Get_First_State (N);
         while S /= No_State loop
            E := Get_First_Src_Edge (S);
            while E /= No_Edge loop
               Put ("-- ");
               Disp_State (S);
               Put (" -> ");
               Disp_State (Get_Edge_Dest (E));
               Put (": ");
               Disp_Psl_Expression (Get_Edge_Expr (E));
               New_Line;
               E := Get_Next_Src_Edge (E);
            end loop;
            S := Get_Next_State (S);
         end loop;
      end if;
   end Disp_PSL_NFA;

   procedure Disp_Psl_Assert_Statement (Stmt : Iir) is
   begin
      Put ("--psl ");
      Disp_Label (Stmt);
      Put ("assert ");
      Disp_Psl_Expression (Get_Psl_Property (Stmt));
      Put_Line (";");
      Disp_PSL_NFA (Get_PSL_NFA (Stmt));
   end Disp_Psl_Assert_Statement;

   procedure Disp_Psl_Cover_Statement (Stmt : Iir) is
   begin
      Put ("--psl ");
      Disp_Label (Stmt);
      Put ("cover ");
      Disp_Psl_Sequence (Get_Psl_Sequence (Stmt));
      Put_Line (";");
      Disp_PSL_NFA (Get_PSL_NFA (Stmt));
   end Disp_Psl_Cover_Statement;

   procedure Disp_Simple_Simultaneous_Statement (Stmt : Iir)
   is
   begin
      Disp_Label (Stmt);
      Disp_Expression (Get_Simultaneous_Left (Stmt));
      Put (" == ");
      Disp_Expression (Get_Simultaneous_Right (Stmt));
      Put_Line (";");
   end Disp_Simple_Simultaneous_Statement;

   procedure Disp_Concurrent_Statement (Stmt: Iir) is
   begin
      case Get_Kind (Stmt) is
         when Iir_Kind_Concurrent_Simple_Signal_Assignment =>
            Disp_Concurrent_Simple_Signal_Assignment (Stmt);
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
            Disp_Concurrent_Conditional_Signal_Assignment (Stmt);
         when Iir_Kind_Concurrent_Selected_Signal_Assignment =>
            Disp_Concurrent_Selected_Signal_Assignment (Stmt);
         when Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement =>
            Disp_Process_Statement (Stmt);
         when Iir_Kind_Concurrent_Assertion_Statement =>
            Disp_Assertion_Statement (Stmt);
         when Iir_Kind_Component_Instantiation_Statement =>
            Disp_Component_Instantiation_Statement (Stmt);
         when Iir_Kind_Concurrent_Procedure_Call_Statement =>
            Disp_Label (Stmt);
            Disp_Postponed (Stmt);
            Disp_Procedure_Call (Get_Procedure_Call (Stmt));
         when Iir_Kind_Block_Statement =>
            Disp_Block_Statement (Stmt);
         when Iir_Kind_If_Generate_Statement =>
            Disp_If_Generate_Statement (Stmt);
         when Iir_Kind_Case_Generate_Statement =>
            Disp_Case_Generate_Statement (Stmt);
         when Iir_Kind_For_Generate_Statement =>
            Disp_For_Generate_Statement (Stmt);
         when Iir_Kind_Psl_Default_Clock =>
            Disp_Psl_Default_Clock (Stmt);
         when Iir_Kind_Psl_Declaration
           | Iir_Kind_Psl_Endpoint_Declaration =>
            Disp_Psl_Declaration (Stmt);
         when Iir_Kind_Psl_Assert_Statement =>
            Disp_Psl_Assert_Statement (Stmt);
         when Iir_Kind_Psl_Cover_Statement =>
            Disp_Psl_Cover_Statement (Stmt);
         when Iir_Kind_Simple_Simultaneous_Statement =>
            Disp_Simple_Simultaneous_Statement (Stmt);
         when others =>
            Error_Kind ("disp_concurrent_statement", Stmt);
      end case;
   end Disp_Concurrent_Statement;

   procedure Disp_Package_Declaration (Decl: Iir_Package_Declaration)
   is
      Header : constant Iir := Get_Package_Header (Decl);
   begin
      Put ("package ");
      Disp_Identifier (Decl);
      Put_Line (" is");
      if Header /= Null_Iir then
         Disp_Generics (Header);
         New_Line;
      end if;
      Disp_Declaration_Chain (Decl, Col + Indentation);
      Disp_End (Decl, "package");
   end Disp_Package_Declaration;

   procedure Disp_Package_Body (Decl: Iir)
   is
   begin
      Put ("package body ");
      Disp_Identifier (Decl);
      Put_Line (" is");
      Disp_Declaration_Chain (Decl, Col + Indentation);
      Disp_End (Decl, "package body");
   end Disp_Package_Body;

   procedure Disp_Package_Instantiation_Declaration (Decl: Iir) is
   begin
      Put ("package ");
      Disp_Identifier (Decl);
      Put_Line (" is new ");
      Disp_Name (Get_Uninstantiated_Package_Name (Decl));
      Put (" ");
      Disp_Generic_Map_Aspect (Decl);
      Put_Line (";");
   end Disp_Package_Instantiation_Declaration;

   procedure Disp_Binding_Indication (Bind : Iir; Indent : Count)
   is
      El : Iir;
   begin
      El := Get_Entity_Aspect (Bind);
      if El /= Null_Iir then
         Set_Col (Indent);
         Put ("use ");
         Disp_Entity_Aspect (El);
      end if;
      El := Get_Generic_Map_Aspect_Chain (Bind);
      if El /= Null_Iir then
         Set_Col (Indent);
         Disp_Generic_Map_Aspect (Bind);
      end if;
      El := Get_Port_Map_Aspect_Chain (Bind);
      if El /= Null_Iir then
         Set_Col (Indent);
         Disp_Port_Map_Aspect (Bind);
      end if;
   end Disp_Binding_Indication;

   procedure Disp_Component_Configuration
     (Conf : Iir_Component_Configuration; Indent : Count)
   is
      Block : Iir_Block_Configuration;
      Binding : Iir;
   begin
      Set_Col (Indent);
      Put ("for ");
      Disp_Instantiation_List (Get_Instantiation_List (Conf));
      Put (" : ");
      Disp_Name (Get_Component_Name (Conf));
      New_Line;
      Binding := Get_Binding_Indication (Conf);
      if Binding /= Null_Iir then
         Disp_Binding_Indication (Binding, Indent + Indentation);
         Put (";");
      end if;
      Block := Get_Block_Configuration (Conf);
      if Block /= Null_Iir then
         Disp_Block_Configuration (Block, Indent + Indentation);
      end if;
      Set_Col (Indent);
      Put_Line ("end for;");
   end Disp_Component_Configuration;

   procedure Disp_Configuration_Items
     (Conf : Iir_Block_Configuration; Indent : Count)
   is
      El : Iir;
   begin
      El := Get_Configuration_Item_Chain (Conf);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Block_Configuration =>
               Disp_Block_Configuration (El, Indent);
            when Iir_Kind_Component_Configuration =>
               Disp_Component_Configuration (El, Indent);
            when Iir_Kind_Configuration_Specification =>
               --  This may be created by canon.
               Set_Col (Indent);
               Disp_Configuration_Specification (El);
               Set_Col (Indent);
               Put_Line ("end for;");
            when others =>
               Error_Kind ("disp_configuration_item_list", El);
         end case;
         El := Get_Chain (El);
      end loop;
   end Disp_Configuration_Items;

   procedure Disp_Block_Configuration
     (Block: Iir_Block_Configuration; Indent: Count)
   is
      Spec : Iir;
   begin
      Set_Col (Indent);
      Put ("for ");
      Spec := Get_Block_Specification (Block);
      case Get_Kind (Spec) is
         when Iir_Kind_Block_Statement
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_Architecture_Body =>
            Disp_Name_Of (Spec);
         when Iir_Kind_Indexed_Name =>
            declare
               Index_List : constant Iir_Flist := Get_Index_List (Spec);
            begin
               Disp_Name_Of (Get_Prefix (Spec));
               Put (" (");
               if Index_List = Iir_Flist_Others then
                  Put ("others");
               else
                  Disp_Expression (Get_Nth_Element (Index_List, 0));
               end if;
               Put (")");
            end;
         when Iir_Kind_Slice_Name =>
            Disp_Name_Of (Get_Prefix (Spec));
            Put (" (");
            Disp_Range (Get_Suffix (Spec));
            Put (")");
         when Iir_Kind_Simple_Name
           | Iir_Kind_Parenthesis_Name =>
            Disp_Name (Spec);
         when others =>
            Error_Kind ("disp_block_configuration", Spec);
      end case;
      New_Line;
      Disp_Declaration_Chain (Block, Indent + Indentation);
      Disp_Configuration_Items (Block, Indent + Indentation);
      Set_Col (Indent);
      Put_Line ("end for;");
   end Disp_Block_Configuration;

   procedure Disp_Configuration_Declaration
     (Decl: Iir_Configuration_Declaration) is
   begin
      Put ("configuration ");
      Disp_Name_Of (Decl);
      Put (" of ");
      Disp_Name (Get_Entity_Name (Decl));
      Put_Line (" is");
      Disp_Declaration_Chain (Decl, Col);
      Disp_Block_Configuration (Get_Block_Configuration (Decl),
                                Col + Indentation);
      Disp_End (Decl, "configuration");
   end Disp_Configuration_Declaration;

   procedure Disp_Context_Items (First : Iir; Indent : Count)
   is
      Decl: Iir;
      Next_Decl : Iir;
   begin
      Decl := First;
      while Decl /= Null_Iir loop
         Next_Decl := Get_Chain (Decl);

         Set_Col (Indent);
         case Get_Kind (Decl) is
            when Iir_Kind_Use_Clause =>
               Disp_Use_Clause (Decl);
            when Iir_Kind_Library_Clause =>
               Put ("library ");
               Disp_Identifier (Decl);
               while Get_Has_Identifier_List (Decl) loop
                  Decl := Next_Decl;
                  Next_Decl := Get_Chain (Decl);
                  Put (", ");
                  Disp_Identifier (Decl);
               end loop;
               Put_Line (";");
            when Iir_Kind_Context_Reference =>
               Put ("context ");
               declare
                  Ref : Iir;
               begin
                  Ref := Decl;
                  loop
                     Disp_Name (Get_Selected_Name (Ref));
                     Ref := Get_Context_Reference_Chain (Ref);
                     exit when Ref = Null_Iir;
                     Put (", ");
                  end loop;
                  Put_Line (";");
               end;
            when others =>
               Error_Kind ("disp_context_items", Decl);
         end case;
         Decl := Next_Decl;
      end loop;
   end Disp_Context_Items;

   procedure Disp_Context_Declaration (Decl: Iir) is
   begin
      Put ("context ");
      Disp_Name_Of (Decl);
      Put_Line (" is");
      Disp_Context_Items (Get_Context_Items (Decl), Col + Indentation);
      Disp_End (Decl, "context");
   end Disp_Context_Declaration;

   procedure Disp_Design_Unit (Unit: Iir_Design_Unit)
   is
      Indent: constant Count := Col;
      Decl: Iir;
   begin
      Disp_Context_Items (Get_Context_Items (Unit), Indent);

      Decl := Get_Library_Unit (Unit);
      Set_Col (Indent);
      case Get_Kind (Decl) is
         when Iir_Kind_Entity_Declaration =>
            Disp_Entity_Declaration (Decl);
         when Iir_Kind_Architecture_Body =>
            Disp_Architecture_Body (Decl);
         when Iir_Kind_Package_Declaration =>
            Disp_Package_Declaration (Decl);
         when Iir_Kind_Package_Body =>
            Disp_Package_Body (Decl);
         when Iir_Kind_Package_Instantiation_Declaration =>
            Disp_Package_Instantiation_Declaration (Decl);
         when Iir_Kind_Configuration_Declaration =>
            Disp_Configuration_Declaration (Decl);
         when Iir_Kind_Context_Declaration =>
            Disp_Context_Declaration (Decl);
         when others =>
            Error_Kind ("disp_design_unit2", Decl);
      end case;
      New_Line;
      New_Line;
   end Disp_Design_Unit;

   procedure Disp_Vhdl (An_Iir: Iir) is
   begin
      -- Put (Count'Image (Line_Length));
      case Get_Kind (An_Iir) is
         when Iir_Kind_Design_Unit =>
            Disp_Design_Unit (An_Iir);
         when Iir_Kind_Character_Literal =>
            Disp_Character_Literal (An_Iir);
         when Iir_Kind_Enumeration_Type_Definition =>
            Disp_Enumeration_Type_Definition (An_Iir);
         when Iir_Kind_Enumeration_Subtype_Definition =>
            Disp_Enumeration_Subtype_Definition (An_Iir);
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
            Disp_Concurrent_Conditional_Signal_Assignment (An_Iir);
         when Iir_Kinds_Dyadic_Operator =>
            Disp_Dyadic_Operator (An_Iir);
         when Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Object_Alias_Declaration =>
            Disp_Name_Of (An_Iir);
         when Iir_Kind_Enumeration_Literal =>
            Disp_Identifier (An_Iir);
         when Iir_Kind_Component_Instantiation_Statement =>
            Disp_Component_Instantiation_Statement (An_Iir);
         when Iir_Kind_Integer_Subtype_Definition =>
            Disp_Integer_Subtype_Definition (An_Iir);
         when Iir_Kind_Array_Subtype_Definition =>
            Disp_Array_Subtype_Definition (An_Iir);
         when Iir_Kind_Array_Type_Definition =>
            Disp_Array_Type_Definition (An_Iir);
         when Iir_Kind_Package_Declaration =>
            Disp_Package_Declaration (An_Iir);
         when Iir_Kind_Wait_Statement =>
            Disp_Wait_Statement (An_Iir);
         when Iir_Kind_Selected_Name
           | Iir_Kind_Selected_Element
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Slice_Name =>
            Disp_Expression (An_Iir);
         when Iir_Kind_Psl_Cover_Statement =>
            Disp_Psl_Cover_Statement (An_Iir);
         when others =>
            Error_Kind ("disp", An_Iir);
      end case;
   end Disp_Vhdl;

   procedure Disp_Int64 (Val: Iir_Int64)
   is
      Str: constant String := Iir_Int64'Image (Val);
   begin
      if Str (Str'First) = ' ' then
         Put (Str (Str'First + 1 .. Str'Last));
      else
         Put (Str);
      end if;
   end Disp_Int64;

   procedure Disp_Int32 (Val: Iir_Int32)
   is
      Str: constant String := Iir_Int32'Image (Val);
   begin
      if Str (Str'First) = ' ' then
         Put (Str (Str'First + 1 .. Str'Last));
      else
         Put (Str);
      end if;
   end Disp_Int32;

   procedure Disp_Fp64 (Val: Iir_Fp64)
   is
      Str: constant String := Iir_Fp64'Image (Val);
   begin
      if Str (Str'First) = ' ' then
         Put (Str (Str'First + 1 .. Str'Last));
      else
         Put (Str);
      end if;
   end Disp_Fp64;
end Disp_Vhdl;
