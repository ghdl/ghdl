--  VHDL regeneration from internal nodes.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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

--  Re-print a tree as VHDL sources.  Except for comments and parenthesis, the
--  sequence of tokens displayed is the same as the sequence of tokens in the
--  input file.  If parenthesis are kept by the parser, the only differences
--  are comments and layout.
with Types; use Types;
with Simple_IO;
with Flags; use Flags;
with Name_Table;
with Str_Table;
with Std_Names; use Std_Names;
with Files_Map;
with File_Comments;

with Vhdl.Types; use Vhdl.Types;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Std_Package;
with PSL.Priorities; use PSL.Priorities;
with PSL.Nodes; use PSL.Nodes;
with PSL.Prints;
with PSL.NFAs;
with PSL.Errors;

package body Vhdl.Prints is

   --  If True, display extra parenthesis to make priority of operators
   --  explicit.
   Flag_Parenthesis : constant Boolean := False;

   --  If set, disp after a string literal the type enclosed into brackets.
   Flag_Disp_String_Literal_Type: constant Boolean := False;

   --  If set, disp implicit declarations.
   Flag_Implicit : constant Boolean := False;

   procedure Disp_Type (Ctxt : in out Ctxt_Class; A_Type: Iir);
   procedure Disp_Range (Ctxt : in out Ctxt_Class; Rng : Iir);
   procedure Print (Ctxt : in out Ctxt_Class; Expr: Iir);

   procedure Disp_Concurrent_Statement (Ctxt : in out Ctxt_Class; Stmt: Iir);
   procedure Disp_Concurrent_Statement_Chain
     (Ctxt : in out Ctxt_Class; Parent: Iir);
   procedure Disp_Simultaneous_Statement_Chain
     (Ctxt : in out Ctxt_Class; Chain: Iir);
   procedure Disp_Declaration_Chain
     (Ctxt : in out Ctxt_Class; Parent : Iir);
   procedure Disp_Process_Statement (Ctxt : in out Ctxt_Class; Process: Iir);
   procedure Disp_Sequential_Statements
     (Ctxt : in out Ctxt_Class; First : Iir);
   procedure Disp_Choice (Ctxt : in out Ctxt_Class; Choice: in out Iir);
   procedure Disp_Association_Chain (Ctxt : in out Ctxt_Class; Chain : Iir);
   procedure Disp_Block_Configuration
     (Ctxt : in out Ctxt_Class; Block: Iir_Block_Configuration);
   procedure Disp_Subprogram_Declaration
     (Ctxt : in out Ctxt_Class; Subprg: Iir; Implicit : Boolean := False);
   procedure Disp_Binding_Indication
     (Ctxt : in out Ctxt_Class; Bind : Iir);
   procedure Disp_Subtype_Indication
     (Ctxt : in out Ctxt_Class; Def : Iir; Full_Decl : Boolean := False);
   procedure Disp_Subnature_Indication (Ctxt : in out Ctxt_Class; Ind : Iir);
   procedure Disp_Parametered_Attribute
     (Ctxt : in out Ctxt_Class; Name : Name_Id; Expr : Iir);
   procedure Disp_String_Literal
     (Ctxt : in out Ctxt_Class; Str : Iir; El_Type : Iir);
   procedure Disp_Package_Declaration
     (Ctxt : in out Ctxt_Class; Decl: Iir_Package_Declaration);
   procedure Disp_Package_Instantiation_Declaration
     (Ctxt : in out Ctxt_Class; Decl: Iir);
   procedure Disp_Package_Body (Ctxt : in out Ctxt_Class; Decl: Iir);
   procedure Disp_Attribute_Name (Ctxt : in out Ctxt_Class; Attr : Iir);

   procedure Print_Property (Ctxt : in out Ctxt_Class;
                             Prop : PSL_Node;
                             Parent_Prio : Priority := Prio_Lowest);
   procedure Print_Sequence (Ctxt : in out Ctxt_Class;
                             Seq : PSL_Node;
                             Parent_Prio : Priority := Prio_Lowest);

   procedure Disp_Int64 (Ctxt : in out Ctxt_Class; Val: Int64);
   procedure Disp_Int32 (Ctxt : in out Ctxt_Class; Val: Iir_Int32);
   procedure Disp_Fp64 (Ctxt : in out Ctxt_Class; Val: Fp64);

   package OOB is
      procedure Put (Str : String);
      procedure New_Line;
   end OOB;

   package body OOB is
      procedure Put (Str : String) is
      begin
         Simple_IO.Put_Err (Str);
      end Put;

      procedure Put (C : Character) is
      begin
         Put ((1 => C));
      end Put;

      procedure New_Line is
      begin
         Put (ASCII.LF);
      end New_Line;
   end OOB;

   procedure Disp_Token (Ctxt : in out Ctxt_Class; Tok1, Tok2 : Token_Type) is
   begin
      Disp_Token (Ctxt, Tok1);
      Disp_Token (Ctxt, Tok2);
   end Disp_Token;

   procedure Disp_Ident (Ctxt : in out Ctxt_Class; Id: Name_Id) is
   begin
      if Name_Table.Is_Character (Id) then
         Start_Lit (Ctxt, Tok_Character);
         Disp_Char (Ctxt, ''');
         Disp_Char (Ctxt, Name_Table.Get_Character (Id));
         Disp_Char (Ctxt, ''');
         Close_Lit (Ctxt);
      else
         Start_Lit (Ctxt, Tok_Identifier);
         if Id = Null_Identifier then
            Disp_Str (Ctxt, "<anonymous>");
         else
            Disp_Str (Ctxt, Name_Table.Image (Id));
         end if;
         Close_Lit (Ctxt);
      end if;
   end Disp_Ident;

   function Or_Else (L, R : Iir) return Iir is
   begin
      if L /= Null_Iir then
         return L;
      end if;
      pragma Assert (R /= Null_Iir);
      return R;
   end Or_Else;

   --  Disp a literal from the sources (so using exactely the same characters).
   procedure Disp_From_Source
     (Ctxt : in out Ctxt_Class;
      Loc : Location_Type; Len : Int32; Tok : Token_Type)
   is
      use Files_Map;
      pragma Assert (Len > 0);
      File : Source_File_Entry;
      Pos : Source_Ptr;
      Buf : File_Buffer_Acc;
   begin
      Location_To_File_Pos (Loc, File, Pos);
      Buf := Get_File_Source (File);
      Start_Lit (Ctxt, Tok);
      for I in 1 .. Len loop
         Disp_Char (Ctxt, Buf (Pos));
         Pos := Pos + 1;
      end loop;
      Close_Lit (Ctxt);
   end Disp_From_Source;

   procedure Disp_Identifier (Ctxt : in out Ctxt_Class; Node : Iir)
   is
      use Name_Table;
      Id : constant Name_Id := Get_Identifier (Node);
      Loc : constant Location_Type := Get_Location (Node);
   begin
      --  Try to display the one from the sources.
      if Id /= Null_Identifier
        and then not Is_Character (Id)
        and then Loc /= No_Location
        and then Loc /= Std_Package.Std_Location
      then
         Disp_From_Source
           (Ctxt, Loc, Int32 (Get_Name_Length (Id)), Tok_Identifier);
      else
         Disp_Ident (Ctxt, Id);
      end if;
   end Disp_Identifier;

   procedure Disp_Literal_From_Source
     (Ctxt : in out Ctxt_Class; Lit : Iir; Tok : Token_Type) is
   begin
      Disp_From_Source
        (Ctxt, Get_Location (Lit), Get_Literal_Length (Lit), Tok);
   end Disp_Literal_From_Source;

   procedure Disp_Function_Name (Ctxt : in out Ctxt_Class; Func: Iir)
   is
      use Name_Table;
      Id : constant Name_Id := Get_Identifier (Func);
   begin
      case Id is
         when Name_Id_Operators
           | Name_Word_Operators
           | Name_Logical_Operators
           | Name_Xnor
           | Name_Shift_Operators =>
            Start_Lit (Ctxt, Tok_String);
            Disp_Char (Ctxt, '"');
            Disp_Str (Ctxt, Image (Id));
            Disp_Char (Ctxt, '"');
            Close_Lit (Ctxt);
         when others =>
            Disp_Ident (Ctxt, Id);
      end case;
   end Disp_Function_Name;

   --  Disp the name of DECL.
   procedure Disp_Name_Of (Ctxt : in out Ctxt_Class; Decl: Iir) is
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Component_Declaration
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Architecture_Body
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Context_Declaration
           | Iir_Kinds_Verification_Unit
           | Iir_Kinds_Interface_Object_Declaration
           | Iir_Kind_Interface_Terminal_Declaration
           | Iir_Kind_Interface_Type_Declaration
           | Iir_Kinds_Interface_Subprogram_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Type_Declaration
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
           | Iir_Kind_Subnature_Declaration
           | Iir_Kind_Terminal_Declaration
           | Iir_Kinds_Quantity_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Character_Literal
           | Iir_Kinds_Process_Statement
           | Iir_Kind_Psl_Endpoint_Declaration
           | Iir_Kind_Psl_Boolean_Parameter =>
            Disp_Identifier (Ctxt, Decl);
         when Iir_Kind_Anonymous_Type_Declaration =>
            Start_Lit (Ctxt, Tok_Identifier);
            Disp_Char (Ctxt, '<');
            Disp_Str (Ctxt, Name_Table.Image (Get_Identifier (Decl)));
            Disp_Char (Ctxt, '>');
            Close_Lit (Ctxt);
         when Iir_Kind_Function_Declaration =>
            Disp_Function_Name (Ctxt, Decl);
         when Iir_Kind_Procedure_Declaration =>
            Disp_Identifier (Ctxt, Decl);
         when Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Protected_Type_Declaration =>
            --  Used for 'end' DECL_NAME.
            Disp_Identifier (Ctxt, Get_Type_Declarator (Decl));
         when Iir_Kind_Record_Nature_Definition =>
            Disp_Identifier (Ctxt, Get_Nature_Declarator (Decl));
         when Iir_Kind_Component_Instantiation_Statement =>
            Disp_Ident (Ctxt, Get_Label (Decl));
         when Iir_Kind_Design_Unit =>
            Disp_Name_Of (Ctxt, Get_Library_Unit (Decl));
         when Iir_Kind_Enumeration_Literal
           | Iir_Kind_Simple_Name =>
            Disp_Identifier (Ctxt, Decl);
         when Iir_Kind_Block_Statement
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_Case_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_Simultaneous_Procedural_Statement =>
            Disp_Ident (Ctxt, Get_Label (Decl));
         when Iir_Kind_Package_Body =>
            Disp_Identifier (Ctxt, Decl);
         when Iir_Kind_Procedure_Body
           | Iir_Kind_Function_Body =>
            Disp_Function_Name (Ctxt, Get_Subprogram_Specification (Decl));
         when Iir_Kind_Protected_Type_Body =>
            Disp_Identifier (Ctxt, Decl);
         when others =>
            Error_Kind ("disp_name_of", Decl);
      end case;
   end Disp_Name_Of;

   procedure Disp_Name_Attribute
     (Ctxt : in out Ctxt_Class; Attr : Iir; Name : Name_Id) is
   begin
      Print (Ctxt, Get_Prefix (Attr));
      Disp_Token (Ctxt, Tok_Tick);
      Disp_Ident (Ctxt, Name);
   end Disp_Name_Attribute;

   procedure Disp_Range (Ctxt : in out Ctxt_Class; Rng : Iir) is
   begin
      case Get_Kind (Rng) is
         when Iir_Kind_Range_Expression =>
            declare
               Origin : constant Iir := Get_Range_Origin (Rng);
            begin
               if Dump_Origin_Flag and then Origin /= Null_Iir then
                  Print (Ctxt, Origin);
               else
                  Print (Ctxt, Or_Else (Get_Left_Limit_Expr (Rng),
                                        Get_Left_Limit (Rng)));
                  if Get_Direction (Rng) = Dir_To then
                     Disp_Token (Ctxt, Tok_To);
                  else
                     Disp_Token (Ctxt, Tok_Downto);
                  end if;
                  Print (Ctxt, Or_Else (Get_Right_Limit_Expr (Rng),
                                        Get_Right_Limit (Rng)));
               end if;
            end;
         when Iir_Kind_Range_Array_Attribute =>
            Disp_Parametered_Attribute (Ctxt, Name_Range, Rng);
         when Iir_Kind_Reverse_Range_Array_Attribute =>
            Disp_Parametered_Attribute (Ctxt, Name_Reverse_Range, Rng);
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Attribute_Name
           | Iir_Kind_Parenthesis_Name =>
            Print (Ctxt, Rng);
         when others =>
            Disp_Subtype_Indication (Ctxt, Rng);
            --  Disp_Name_Of (Get_Type_Declarator (Decl));
      end case;
   end Disp_Range;

   procedure Disp_After_End
     (Ctxt : in out Ctxt_Class;
      Decl : Iir; Tok1 : Token_Type; Tok2 : Token_Type := Tok_Invalid) is
   begin
      if Get_End_Has_Reserved_Id (Decl) then
         Disp_Token (Ctxt, Tok1);
         if Tok2 /= Tok_Invalid then
            Disp_Token (Ctxt, Tok2);
         end if;
      end if;
      if Get_End_Has_Identifier (Decl) then
         Disp_Name_Of (Ctxt, Decl);
      end if;
   end Disp_After_End;

   procedure Disp_End_No_Close
     (Ctxt : in out Ctxt_Class;
      Decl : Iir; Tok1 : Token_Type; Tok2 : Token_Type := Tok_Invalid) is
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_End);
      Disp_After_End (Ctxt, Decl, Tok1, Tok2);
   end Disp_End_No_Close;

   procedure Disp_End
     (Ctxt : in out Ctxt_Class;
      Decl : Iir; Tok1 : Token_Type; Tok2 : Token_Type := Tok_Invalid) is
   begin
      Disp_End_No_Close (Ctxt, Decl, Tok1, Tok2);
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_End;

   procedure Disp_End (Ctxt : in out Ctxt_Class; Tok1 : Token_Type) is
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_End);
      Disp_Token (Ctxt, Tok1);
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_End;

   procedure Disp_End_Label_No_Close
     (Ctxt : in out Ctxt_Class; Stmt : Iir; Tok : Token_Type) is
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_End);
      Disp_Token (Ctxt, Tok);
      if Get_End_Has_Identifier (Stmt) then
         Disp_Ident (Ctxt, Get_Label (Stmt));
      end if;
      Disp_Token (Ctxt, Tok_Semi_Colon);
   end Disp_End_Label_No_Close;

   procedure Disp_End_Label
     (Ctxt : in out Ctxt_Class; Stmt : Iir; Tok : Token_Type) is
   begin
      Disp_End_Label_No_Close (Ctxt, Stmt, Tok);
      Close_Hbox (Ctxt);
   end Disp_End_Label;

   procedure Disp_Use_Clause (Ctxt : in out Ctxt_Class; Clause: Iir_Use_Clause)
   is
      Name : Iir;
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Use);
      Name := Clause;
      loop
         Print (Ctxt, Get_Selected_Name (Name));
         Name := Get_Use_Clause_Chain (Name);
         exit when Name = Null_Iir;
         Disp_Token (Ctxt, Tok_Comma);
      end loop;
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Use_Clause;

   -- Disp the resolution function (if any) of type definition DEF.
   procedure Disp_Resolution_Indication
     (Ctxt : in out Ctxt_Class; Subtype_Def: Iir)
   is
      procedure Inner (Ind : Iir) is
      begin
         case Get_Kind (Ind) is
            when Iir_Kinds_Denoting_Name =>
               Print (Ctxt, Ind);
            when Iir_Kind_Array_Element_Resolution =>
               declare
                  Res : constant Iir := Get_Resolution_Indication (Ind);
               begin
                  Disp_Token (Ctxt, Tok_Left_Paren);
                  if Is_Valid (Res) then
                     Inner (Res);
                  else
                     Print (Ctxt, Get_Resolution_Indication
                                  (Get_Element_Subtype_Indication (Ind)));
                  end if;
                  Disp_Token (Ctxt, Tok_Right_Paren);
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

      if False then
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
      end if;

      Inner (Ind);
   end Disp_Resolution_Indication;

   procedure Disp_Element_Constraint
     (Ctxt : in out Ctxt_Class; Def : Iir; Type_Mark : Iir);

   procedure Disp_Discrete_Range
     (Ctxt : in out Ctxt_Class; Iterator: Iir) is
   begin
      if Get_Kind (Iterator) in Iir_Kinds_Subtype_Definition then
         Disp_Subtype_Indication (Ctxt, Iterator);
      else
         Disp_Range (Ctxt, Iterator);
      end if;
   end Disp_Discrete_Range;

   procedure Disp_Array_Sub_Definition_Indexes
     (Ctxt : in out Ctxt_Class; Def : Iir)
   is
      Indexes : Iir_Flist;
      Index : Iir;
   begin
      Indexes := Get_Index_Constraint_List (Def);
      if Indexes = Null_Iir_Flist then
         Indexes := Get_Index_Subtype_List (Def);
      end if;
      Disp_Token (Ctxt, Tok_Left_Paren);
      for I in Flist_First .. Flist_Last (Indexes) loop
         Index := Get_Nth_Element (Indexes, I);
         if I /= 0 then
            Disp_Token (Ctxt, Tok_Comma);
         end if;
         Disp_Discrete_Range (Ctxt, Index);
      end loop;
      Disp_Token (Ctxt, Tok_Right_Paren);
   end Disp_Array_Sub_Definition_Indexes;

   procedure Disp_Array_Element_Constraint
     (Ctxt : in out Ctxt_Class; Def : Iir; Type_Mark : Iir) is
   begin
      if not Get_Has_Array_Constraint_Flag (Def)
        and then not Get_Has_Element_Constraint_Flag (Def)
      then
         return;
      end if;

      if Get_Has_Array_Constraint_Flag (Def) then
         if Get_Index_Constraint_List (Def) = Null_Iir_Flist then
            Disp_Token (Ctxt, Tok_Left_Paren);
            Disp_Token (Ctxt, Tok_Open);
            Disp_Token (Ctxt, Tok_Right_Paren);
         else
            Disp_Array_Sub_Definition_Indexes (Ctxt, Def);
         end if;
      end if;

      if Get_Has_Element_Constraint_Flag (Def) then
         Disp_Element_Constraint (Ctxt, Get_Array_Element_Constraint (Def),
                                  Get_Element_Subtype (Type_Mark));
      end if;
   end Disp_Array_Element_Constraint;

   procedure Disp_Record_Element_Constraint
     (Ctxt : in out Ctxt_Class; Def : Iir)
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
               Disp_Token (Ctxt, Tok_Comma);
            else
               Disp_Token (Ctxt, Tok_Left_Paren);
               Has_El := True;
            end if;
            Disp_Name_Of (Ctxt, El);
            Disp_Element_Constraint (Ctxt, Get_Type (El),
                                     Get_Base_Type (Get_Type (El)));
         end if;
      end loop;
      if Has_El then
         Disp_Token (Ctxt, Tok_Right_Paren);
      end if;
   end Disp_Record_Element_Constraint;

   procedure Disp_Element_Constraint
     (Ctxt : in out Ctxt_Class; Def : Iir; Type_Mark : Iir) is
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Record_Subtype_Definition =>
            Disp_Record_Element_Constraint (Ctxt, Def);
         when Iir_Kind_Array_Subtype_Definition =>
            Disp_Array_Element_Constraint (Ctxt, Def, Type_Mark);
         when others =>
            Error_Kind ("disp_element_constraint", Def);
      end case;
   end Disp_Element_Constraint;

   procedure Disp_Tolerance_Opt (Ctxt : in out Ctxt_Class; N : Iir)
   is
      Tol : constant Iir := Get_Tolerance (N);
   begin
      if Tol /= Null_Iir then
         Disp_Token (Ctxt, Tok_Tolerance);
         Print (Ctxt, Tol);
      end if;
   end Disp_Tolerance_Opt;

   procedure Disp_Subtype_Indication
     (Ctxt : in out Ctxt_Class; Def : Iir; Full_Decl : Boolean := False)
   is
      Type_Mark : Iir;
      Base_Type : Iir;
      Decl : Iir;
   begin
      case Get_Kind (Def) is
         when Iir_Kinds_Denoting_Name
           | Iir_Kind_Subtype_Attribute
           | Iir_Kind_Attribute_Name =>
            Print (Ctxt, Def);
            return;
         when others =>
            null;
      end case;

      Decl := Get_Type_Declarator (Def);
      if not Full_Decl and then Decl /= Null_Iir then
         Disp_Name_Of (Ctxt, Decl);
         return;
      end if;

      -- Resolution function name.
      Disp_Resolution_Indication (Ctxt, Def);

      -- type mark.
      Type_Mark := Get_Subtype_Type_Mark (Def);
      if Type_Mark /= Null_Iir then
         Print (Ctxt, Type_Mark);
         Type_Mark := Get_Type (Type_Mark);
      end if;

      case Get_Kind (Def) is
         when Iir_Kind_Array_Subtype_Definition =>
            Disp_Array_Element_Constraint
              (Ctxt, Def, Or_Else (Type_Mark, Def));
         when Iir_Kind_Subtype_Definition =>
            declare
               Rng : constant Iir := Get_Range_Constraint (Def);
            begin
               if Rng /= Null_Iir then
                  Disp_Token (Ctxt, Tok_Range);
                  Print (Ctxt, Get_Range_Constraint (Def));
               end if;
               Disp_Tolerance_Opt (Ctxt, Def);
            end;
         when others =>
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
                        Disp_Token (Ctxt, Tok_Range);
                     end if;
                     Print (Ctxt, Get_Range_Constraint (Def));
                  end if;
                  if Get_Kind (Base_Type) = Iir_Kind_Floating_Type_Definition
                  then
                     Disp_Tolerance_Opt (Ctxt, Def);
                  end if;
               when Iir_Kind_Access_Type_Definition =>
                  declare
                     Des_Ind : constant Iir :=
                       Get_Designated_Subtype_Indication (Def);
                  begin
                     if Des_Ind /= Null_Iir then
                        pragma Assert (Get_Kind (Des_Ind)
                                         = Iir_Kind_Array_Subtype_Definition);
                        Disp_Array_Element_Constraint
                          (Ctxt, Des_Ind, Get_Designated_Type (Base_Type));
                     end if;
                  end;
               when Iir_Kind_Array_Type_Definition =>
                  Disp_Array_Element_Constraint
                    (Ctxt, Def, Or_Else (Type_Mark, Def));
               when Iir_Kind_Record_Type_Definition =>
                  Disp_Record_Element_Constraint (Ctxt, Def);
               when others =>
                  Error_Kind ("disp_subtype_indication", Base_Type);
            end case;
      end case;
   end Disp_Subtype_Indication;

   procedure Disp_Enumeration_Type_Definition
     (Ctxt : in out Ctxt_Class; Def: Iir_Enumeration_Type_Definition)
   is
      Lits : constant Iir_Flist := Get_Enumeration_Literal_List (Def);
      A_Lit: Iir;
   begin
      Disp_Token (Ctxt, Tok_Left_Paren);
      for I in Flist_First .. Flist_Last (Lits) loop
         A_Lit := Get_Nth_Element (Lits, I);
         if I > 0 then
            Disp_Token (Ctxt, Tok_Comma);
         end if;
         Disp_Name_Of (Ctxt, A_Lit);
      end loop;
      Disp_Token (Ctxt, Tok_Right_Paren);
   end Disp_Enumeration_Type_Definition;

   procedure Disp_Array_Definition_Indexes
     (Ctxt : in out Ctxt_Class; Def: Iir)
   is
      Indexes : Iir_Flist;
      Index: Iir;
   begin
      Indexes := Get_Index_Subtype_Definition_List (Def);
      if Indexes = Null_Iir_Flist then
         Indexes := Get_Index_Subtype_List (Def);
      end if;
      Disp_Token (Ctxt, Tok_Array, Tok_Left_Paren);
      for I in Flist_First .. Flist_Last (Indexes) loop
         Index := Get_Nth_Element (Indexes, I);
         if I /= 0 then
            Disp_Token (Ctxt, Tok_Comma);
         end if;
         Print (Ctxt, Index);
         Disp_Token (Ctxt, Tok_Range, Tok_Box);
      end loop;
      Disp_Token (Ctxt, Tok_Right_Paren, Tok_Of);
   end Disp_Array_Definition_Indexes;

   procedure Disp_Array_Type_Definition
     (Ctxt : in out Ctxt_Class; Def: Iir_Array_Type_Definition) is
   begin
      Disp_Array_Definition_Indexes (Ctxt, Def);
      Disp_Subtype_Indication (Ctxt, Get_Element_Subtype_Indication (Def));
   end Disp_Array_Type_Definition;

   procedure Disp_Physical_Literal (Ctxt : in out Ctxt_Class; Lit: Iir)
   is
      Len : constant Int32 := Get_Literal_Length (Lit);
      Unit : Iir;
   begin
      case Iir_Kinds_Physical_Literal (Get_Kind (Lit)) is
         when Iir_Kind_Physical_Int_Literal =>
            if Len /= 0 then
               Disp_Literal_From_Source (Ctxt, Lit, Tok_Integer);
            else
               Disp_Int64 (Ctxt, Get_Value (Lit));
            end if;
         when Iir_Kind_Physical_Fp_Literal =>
            if Len /= 0 then
               Disp_Literal_From_Source (Ctxt, Lit, Tok_Real);
            else
               Disp_Fp64 (Ctxt, Get_Fp_Value (Lit));
            end if;
      end case;

      Unit := Get_Unit_Name (Lit);
      if Is_Valid (Unit) then
         --  No unit in range_constraint of physical type declaration.
         Print (Ctxt, Unit);
      end if;
   end Disp_Physical_Literal;

   procedure Disp_Record_Type_Definition
     (Ctxt : in out Ctxt_Class; Def: Iir_Record_Type_Definition)
   is
      List : constant Iir_Flist := Get_Elements_Declaration_List (Def);
      El: Iir_Element_Declaration;
      El_Subtype : Iir;
      Reindent : Boolean;
   begin
      Disp_Token (Ctxt, Tok_Record);
      Close_Hbox (Ctxt);
      Reindent := True;
      Start_Vbox (Ctxt);
      for I in Flist_First .. Flist_Last (List) loop
         El := Get_Nth_Element (List, I);
         if Reindent then
            El_Subtype := Get_Subtype_Indication (El);
            Start_Hbox (Ctxt);
         end if;
         Disp_Identifier (Ctxt, El);
         if Get_Has_Identifier_List (El) then
            Disp_Token (Ctxt, Tok_Comma);
            Reindent := False;
         else
            Disp_Token (Ctxt, Tok_Colon);
            Disp_Subtype_Indication (Ctxt, Or_Else (El_Subtype,
                                                    Get_Type (El)));
            Disp_Token (Ctxt, Tok_Semi_Colon);
            Close_Hbox (Ctxt);
            Reindent := True;
         end if;
      end loop;
      Close_Vbox (Ctxt);
      Disp_End_No_Close (Ctxt, Def, Tok_Record);
   end Disp_Record_Type_Definition;

   procedure Disp_Designator_List (Ctxt : in out Ctxt_Class; List: Iir_List)
   is
      El : Iir;
      It : List_Iterator;
      Is_First : Boolean;
   begin
      case List is
         when Null_Iir_List =>
            null;
         when Iir_List_All =>
            Disp_Token (Ctxt, Tok_All);
         when others =>
            It := List_Iterate (List);
            Is_First := True;
            while Is_Valid (It) loop
               El := Get_Element (It);
               if not Is_First then
                  Disp_Token (Ctxt, Tok_Comma);
               else
                  Is_First := False;
               end if;
               Print (Ctxt, El);
               Next (It);
            end loop;
      end case;
   end Disp_Designator_List;

   procedure Disp_Array_Subtype_Definition
     (Ctxt : in out Ctxt_Class; Def : Iir; El_Def : Iir) is
   begin
      Disp_Token (Ctxt, Tok_Array);
      Disp_Array_Sub_Definition_Indexes (Ctxt, Def);
      Disp_Token (Ctxt, Tok_Of);
      Disp_Subtype_Indication (Ctxt, El_Def);
   end Disp_Array_Subtype_Definition;

   -- Display the full definition of a type, ie the sequence that can create
   -- such a type.
   procedure Disp_Type_Definition (Ctxt : in out Ctxt_Class; Def: Iir) is
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Enumeration_Type_Definition =>
            Disp_Enumeration_Type_Definition (Ctxt, Def);
         when Iir_Kind_Array_Type_Definition =>
            Disp_Array_Type_Definition (Ctxt, Def);
         when Iir_Kind_Array_Subtype_Definition =>
            Disp_Array_Subtype_Definition
              (Ctxt, Def, Get_Element_Subtype (Get_Base_Type (Def)));
         when Iir_Kind_Record_Type_Definition =>
            Disp_Record_Type_Definition (Ctxt, Def);
         when Iir_Kind_Access_Type_Definition =>
            Disp_Token (Ctxt, Tok_Access);
            Disp_Subtype_Indication
              (Ctxt, Get_Designated_Subtype_Indication (Def));
         when Iir_Kind_File_Type_Definition =>
            Disp_Token (Ctxt, Tok_File, Tok_Of);
            Disp_Subtype_Indication (Ctxt, Get_File_Type_Mark (Def));
         when Iir_Kind_Protected_Type_Declaration =>
            Disp_Token (Ctxt, Tok_Protected);
            Close_Hbox (Ctxt);
            Start_Vbox (Ctxt);
            Disp_Declaration_Chain (Ctxt, Def);
            Close_Vbox (Ctxt);
            Disp_End_No_Close (Ctxt, Def, Tok_Protected);
         when Iir_Kind_Attribute_Name
           | Iir_Kind_Range_Expression
           | Iir_Kind_Parenthesis_Name =>
            Disp_Token (Ctxt, Tok_Range);
            Print (Ctxt, Def);
         when others =>
            Error_Kind ("disp_type_definition", Def);
      end case;
   end Disp_Type_Definition;

   procedure Disp_Type_Declaration
     (Ctxt : in out Ctxt_Class; Decl: Iir_Type_Declaration)
   is
      Def : constant Iir := Get_Type_Definition (Decl);
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Type);
      Disp_Name_Of (Ctxt, Decl);
      if Def /= Null_Iir
        and then Get_Kind (Def) /= Iir_Kind_Incomplete_Type_Definition
      then
         Disp_Token (Ctxt, Tok_Is);
         Disp_Type_Definition (Ctxt, Def);
      end if;
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Type_Declaration;

   procedure Disp_Physical_Type_Definition
     (Ctxt : in out Ctxt_Class; Decl : Iir)
   is
      Def : constant Iir := Get_Type_Definition (Decl);
      St : constant Iir := Get_Subtype_Definition (Decl);
      Unit : Iir_Unit_Declaration;
      Rng : Iir;
   begin
      Disp_Token (Ctxt, Tok_Range);
      Rng := Or_Else (St, Def);
      Print (Ctxt, Get_Range_Constraint (Rng));
      Disp_Token (Ctxt, Tok_Units);
      Close_Hbox (Ctxt);

      Start_Vbox (Ctxt);
      Unit := Get_Unit_Chain (Def);
      Start_Hbox (Ctxt);
      Disp_Identifier (Ctxt, Unit);
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
      Unit := Get_Chain (Unit);
      while Unit /= Null_Iir loop
         Start_Hbox (Ctxt);
         Disp_Identifier (Ctxt, Unit);
         Disp_Token (Ctxt, Tok_Equal);
         Print (Ctxt, Get_Physical_Literal (Unit));
         Disp_Token (Ctxt, Tok_Semi_Colon);
         Close_Hbox (Ctxt);
         Unit := Get_Chain (Unit);
      end loop;
      Close_Vbox (Ctxt);
      Disp_End_No_Close (Ctxt, Def, Tok_Units);
   end Disp_Physical_Type_Definition;

   procedure Disp_Anonymous_Type_Declaration
     (Ctxt : in out Ctxt_Class; Decl: Iir_Anonymous_Type_Declaration)
   is
      Def : constant Iir := Get_Type_Definition (Decl);
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Type);
      Disp_Identifier (Ctxt, Decl);
      Disp_Token (Ctxt, Tok_Is);
      case Get_Kind (Def) is
         when Iir_Kind_Array_Type_Definition =>
            Disp_Array_Subtype_Definition
              (Ctxt, Get_Subtype_Definition (Decl),
               Get_Element_Subtype_Indication (Def));
         when Iir_Kind_Array_Subtype_Definition =>
            Disp_Array_Subtype_Definition
              (Ctxt, Def, Get_Array_Element_Constraint (Def));
         when Iir_Kind_Physical_Type_Definition =>
            Disp_Physical_Type_Definition (Ctxt, Decl);
         when Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Integer_Type_Definition =>
            declare
               St : constant Iir := Get_Subtype_Definition (Decl);
            begin
               Disp_Token (Ctxt, Tok_Range);
               Print (Ctxt, Get_Range_Constraint (St));
            end;
         when others =>
            Disp_Type_Definition (Ctxt, Def);
      end case;
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Anonymous_Type_Declaration;

   procedure Disp_Subtype_Declaration
     (Ctxt : in out Ctxt_Class; Decl: Iir_Subtype_Declaration)
   is
      Def : constant Iir := Get_Type (Decl);
   begin
      --  If the subtype declaration was implicit (added because of a type
      --  declaration), put it as a comment.
      if Def /= Null_Iir
        and then
        (Get_Identifier (Decl)
           = Get_Identifier (Get_Type_Declarator (Get_Base_Type (Def))))
      then
         if Flag_Implicit then
            OOB.Put ("-- ");
         else
            return;
         end if;
      end if;
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Subtype);
      Disp_Name_Of (Ctxt, Decl);
      Disp_Token (Ctxt, Tok_Is);
      Disp_Subtype_Indication
        (Ctxt, Or_Else (Get_Subtype_Indication (Decl), Get_Type (Decl)), True);
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Subtype_Declaration;

   procedure Disp_Type (Ctxt : in out Ctxt_Class; A_Type: Iir)
   is
      Decl: Iir;
   begin
      Decl := Get_Type_Declarator (A_Type);
      if Decl /= Null_Iir then
         Disp_Name_Of (Ctxt, Decl);
      else
         case Get_Kind (A_Type) is
            when Iir_Kind_Enumeration_Type_Definition
              | Iir_Kind_Integer_Type_Definition =>
               raise Program_Error;
            when Iir_Kind_Integer_Subtype_Definition
              | Iir_Kind_Enumeration_Subtype_Definition
              | Iir_Kind_Access_Subtype_Definition =>
               Disp_Subtype_Indication (Ctxt, A_Type);
            when Iir_Kind_Array_Subtype_Definition =>
               Disp_Subtype_Indication (Ctxt, A_Type);
            when others =>
               Error_Kind ("disp_type", A_Type);
         end case;
      end if;
   end Disp_Type;

   procedure Disp_Scalar_Nature_Definition
     (Ctxt : in out Ctxt_Class; Def : Iir) is
   begin
      Print (Ctxt, Get_Across_Type_Mark (Def));
      Disp_Token (Ctxt, Tok_Across);
      Print (Ctxt, Get_Through_Type_Mark (Def));
      Disp_Token (Ctxt, Tok_Through);
      Disp_Name_Of (Ctxt, Get_Reference (Def));
      Disp_Token (Ctxt, Tok_Reference);
   end Disp_Scalar_Nature_Definition;

   procedure Disp_Array_Nature_Definition
     (Ctxt : in out Ctxt_Class; Def: Iir) is
   begin
      Disp_Array_Definition_Indexes (Ctxt, Def);
      Disp_Subnature_Indication (Ctxt, Get_Element_Subnature_Indication (Def));
   end Disp_Array_Nature_Definition;

   procedure Disp_Record_Nature_Definition
     (Ctxt : in out Ctxt_Class; Def : Iir)
   is
      List : constant Iir_Flist := Get_Elements_Declaration_List (Def);
      El: Iir_Element_Declaration;
      El_Subnature : Iir;
      Reindent : Boolean;
   begin
      Disp_Token (Ctxt, Tok_Record);
      Close_Hbox (Ctxt);
      Reindent := True;
      Start_Vbox (Ctxt);
      for I in Flist_First .. Flist_Last (List) loop
         El := Get_Nth_Element (List, I);
         if Reindent then
            El_Subnature := Get_Subnature_Indication (El);
            Start_Hbox (Ctxt);
         end if;
         Disp_Identifier (Ctxt, El);
         if Get_Has_Identifier_List (El) then
            Disp_Token (Ctxt, Tok_Comma);
            Reindent := False;
         else
            Disp_Token (Ctxt, Tok_Colon);
            Disp_Subnature_Indication (Ctxt, El_Subnature);
            Disp_Token (Ctxt, Tok_Semi_Colon);
            Close_Hbox (Ctxt);
            Reindent := True;
         end if;
      end loop;
      Close_Vbox (Ctxt);
      Disp_End_No_Close (Ctxt, Def, Tok_Record);
   end Disp_Record_Nature_Definition;

   procedure Disp_Nature_Definition (Ctxt : in out Ctxt_Class; Def : Iir) is
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Scalar_Nature_Definition =>
            Disp_Scalar_Nature_Definition (Ctxt, Def);
         when Iir_Kind_Record_Nature_Definition =>
            Disp_Record_Nature_Definition (Ctxt, Def);
         when Iir_Kind_Array_Nature_Definition =>
            Disp_Array_Nature_Definition (Ctxt, Def);
         when others =>
            Error_Kind ("disp_nature_definition", Def);
      end case;
   end Disp_Nature_Definition;

   procedure Disp_Nature_Declaration (Ctxt : in out Ctxt_Class; Decl : Iir) is
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Nature);
      Disp_Identifier (Ctxt, Decl);
      Disp_Token (Ctxt, Tok_Is);
      Disp_Nature_Definition (Ctxt, Get_Nature (Decl));
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Nature_Declaration;

   procedure Disp_Subnature_Indication (Ctxt : in out Ctxt_Class; Ind : Iir) is
   begin
      case Get_Kind (Ind) is
         when Iir_Kinds_Denoting_Name
           | Iir_Kind_Subtype_Attribute
           | Iir_Kind_Attribute_Name =>
            Print (Ctxt, Ind);
         when Iir_Kind_Array_Subnature_Definition =>
            Print (Ctxt, Get_Subnature_Nature_Mark (Ind));
            Disp_Array_Sub_Definition_Indexes (Ctxt, Ind);
         when others =>
            Error_Kind ("disp_subnature_indication", Ind);
      end case;
   end Disp_Subnature_Indication;

   procedure Disp_Subnature_Declaration
     (Ctxt : in out Ctxt_Class; Decl : Iir) is
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Subnature);
      Disp_Identifier (Ctxt, Decl);
      Disp_Token (Ctxt, Tok_Is);
      Disp_Subnature_Indication (Ctxt, Get_Subnature_Indication (Decl));
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Subnature_Declaration;

   procedure Disp_Mode (Ctxt : in out Ctxt_Class; Mode: Iir_Mode) is
   begin
      case Mode is
         when Iir_In_Mode =>
            Disp_Token (Ctxt, Tok_In);
         when Iir_Out_Mode =>
            Disp_Token (Ctxt, Tok_Out);
         when Iir_Inout_Mode =>
            Disp_Token (Ctxt, Tok_Inout);
         when Iir_Buffer_Mode =>
            Disp_Token (Ctxt, Tok_Buffer);
         when Iir_Linkage_Mode =>
            Disp_Token (Ctxt, Tok_Linkage);
         when Iir_Unknown_Mode =>
            null;
      end case;
   end Disp_Mode;

   procedure Disp_Signal_Kind (Ctxt : in out Ctxt_Class; Sig : Iir) is
   begin
      if Get_Guarded_Signal_Flag (Sig) then
         case Get_Signal_Kind (Sig) is
            when Iir_Register_Kind =>
               Disp_Token (Ctxt, Tok_Register);
            when Iir_Bus_Kind =>
               Disp_Token (Ctxt, Tok_Bus);
         end case;
      end if;
   end Disp_Signal_Kind;

   procedure Disp_Interface_Class (Ctxt : in out Ctxt_Class; Inter: Iir) is
   begin
      if Get_Has_Class (Inter) then
         case Get_Kind (Inter) is
            when Iir_Kind_Interface_Signal_Declaration =>
               Disp_Token (Ctxt, Tok_Signal);
            when Iir_Kind_Interface_Variable_Declaration =>
               Disp_Token (Ctxt, Tok_Variable);
            when Iir_Kind_Interface_Constant_Declaration =>
               Disp_Token (Ctxt, Tok_Constant);
            when Iir_Kind_Interface_File_Declaration =>
               Disp_Token (Ctxt, Tok_File);
            when Iir_Kind_Interface_Terminal_Declaration =>
               Disp_Token (Ctxt, Tok_Terminal);
            when Iir_Kind_Interface_Quantity_Declaration =>
               Disp_Token (Ctxt, Tok_Quantity);
            when others =>
               Error_Kind ("disp_interface_class", Inter);
         end case;
      end if;
   end Disp_Interface_Class;

   procedure Disp_Default_Value_Opt (Ctxt : in out Ctxt_Class; Obj : Iir)
   is
      Default: constant Iir := Get_Default_Value (Obj);
   begin
      if Default /= Null_Iir then
         Valign (Ctxt, Valign_Assign);
         Disp_Token (Ctxt, Tok_Assign);
         Print (Ctxt, Default);
      end if;
   end Disp_Default_Value_Opt;

   procedure Disp_Interface_Mode_And_Type
     (Ctxt : in out Ctxt_Class; Inter: Iir)
   is
      Ind : constant Iir := Get_Subtype_Indication (Inter);
   begin
      Valign (Ctxt, Valign_Colon);
      Disp_Token (Ctxt, Tok_Colon);
      if Get_Has_Mode (Inter) then
         Disp_Mode (Ctxt, Get_Mode (Inter));
      end if;
      Valign (Ctxt, Valign_Typemark);
      if Ind = Null_Iir then
         --  For implicit subprogram
         Disp_Type (Ctxt, Get_Type (Inter));
      else
         Disp_Subtype_Indication (Ctxt, Get_Subtype_Indication (Inter));
      end if;
      if Get_Kind (Inter) = Iir_Kind_Interface_Signal_Declaration then
         Disp_Signal_Kind (Ctxt, Inter);
      end if;
      Disp_Default_Value_Opt (Ctxt, Inter);
   end Disp_Interface_Mode_And_Type;

   --  Disp interfaces, followed by END_STR (';' in general).
   procedure Disp_Interface_Chain
     (Ctxt : in out Ctxt_Class; Chain: Iir; With_Box : Boolean)
   is
      Inter: Iir;
      Next_Inter : Iir;
      First_Inter : Iir;
   begin
      if Chain = Null_Iir then
         return;
      end if;
      Disp_Token (Ctxt, Tok_Left_Paren);

      if With_Box then
         Close_Hbox (Ctxt);
         Start_Vbox (Ctxt);
      end if;

      Inter := Chain;
      loop
         Next_Inter := Get_Chain (Inter);

         First_Inter := Inter;

         Start_Node (Ctxt, Inter);

         if With_Box then
            Start_Hbox (Ctxt);
         end if;

         case Iir_Kinds_Interface_Declaration (Get_Kind (Inter)) is
            when Iir_Kinds_Interface_Object_Declaration =>
               Disp_Interface_Class (Ctxt, Inter);
               Disp_Name_Of (Ctxt, Inter);
               while Get_Has_Identifier_List (Inter) loop
                  Disp_Token (Ctxt, Tok_Comma);
                  Inter := Next_Inter;
                  Next_Inter := Get_Chain (Inter);
                  Disp_Name_Of (Ctxt, Inter);
               end loop;
               Disp_Interface_Mode_And_Type (Ctxt, First_Inter);
            when Iir_Kind_Interface_Terminal_Declaration =>
               Disp_Interface_Class (Ctxt, Inter);
               Disp_Name_Of (Ctxt, Inter);
               while Get_Has_Identifier_List (Inter) loop
                  Disp_Token (Ctxt, Tok_Comma);
                  Inter := Next_Inter;
                  Next_Inter := Get_Chain (Inter);
                  Disp_Name_Of (Ctxt, Inter);
               end loop;
               Disp_Token (Ctxt, Tok_Colon);
               Disp_Subnature_Indication
                 (Ctxt, Get_Subnature_Indication (First_Inter));
            when Iir_Kind_Interface_Package_Declaration =>
               Disp_Token (Ctxt, Tok_Package);
               Disp_Identifier (Ctxt, Inter);
               Disp_Token (Ctxt, Tok_Is, Tok_New);
               Print (Ctxt, Get_Uninstantiated_Package_Name (Inter));
               Disp_Token (Ctxt, Tok_Generic, Tok_Map);
               declare
                  Assoc_Chain : constant Iir :=
                    Get_Generic_Map_Aspect_Chain (Inter);
               begin
                  if Assoc_Chain = Null_Iir then
                     Disp_Token (Ctxt, Tok_Left_Paren);
                     Disp_Token (Ctxt, Tok_Box);
                     Disp_Token (Ctxt, Tok_Right_Paren);
                  else
                     Disp_Association_Chain (Ctxt, Assoc_Chain);
                  end if;
               end;
            when Iir_Kind_Interface_Type_Declaration =>
               Disp_Token (Ctxt, Tok_Type);
               Disp_Identifier (Ctxt, Inter);
            when Iir_Kinds_Interface_Subprogram_Declaration =>
               Disp_Subprogram_Declaration (Ctxt, Inter);
            --  when others =>
            --     Error_Kind ("disp_interface_chain", Inter);
         end case;

         if Next_Inter /= Null_Iir then
            Disp_Token (Ctxt, Tok_Semi_Colon);
         end if;

         if With_Box then
            Close_Hbox (Ctxt);
         end if;

         exit when Next_Inter = Null_Iir;

         Inter := Next_Inter;
         Next_Inter := Get_Chain (Inter);
      end loop;

      if With_Box then
         Close_Vbox (Ctxt);
         Start_Hbox (Ctxt);
      end if;

      Disp_Token (Ctxt, Tok_Right_Paren);
   end Disp_Interface_Chain;

   procedure Disp_Ports (Ctxt : in out Ctxt_Class; Parent : Iir)
   is
      Ports : constant Iir := Get_Port_Chain (Parent);
   begin
      if Ports /= Null_Iir then
         Start_Hbox (Ctxt);
         Disp_Token (Ctxt, Tok_Port);
         Disp_Interface_Chain (Ctxt, Ports, True);
         Disp_Token (Ctxt, Tok_Semi_Colon);
         Close_Hbox (Ctxt);
      end if;
   end Disp_Ports;

   procedure Disp_Generics (Ctxt : in out Ctxt_Class; Parent : Iir)
   is
      Generics : constant Iir := Get_Generic_Chain (Parent);
   begin
      if Generics /= Null_Iir then
         Start_Hbox (Ctxt);
         Disp_Token (Ctxt, Tok_Generic);
         Disp_Interface_Chain (Ctxt, Generics, True);
         Disp_Token (Ctxt, Tok_Semi_Colon);
         Close_Hbox (Ctxt);
      end if;
   end Disp_Generics;

   procedure Disp_Entity_Declaration
     (Ctxt : in out Ctxt_Class; Decl: Iir_Entity_Declaration) is
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Entity);
      Disp_Name_Of (Ctxt, Decl);
      Disp_Token (Ctxt, Tok_Is);
      Close_Hbox (Ctxt);

      Start_Vbox (Ctxt);
      Disp_Generics (Ctxt, Decl);
      Disp_Ports (Ctxt, Decl);
      Disp_Declaration_Chain (Ctxt, Decl);
      Close_Vbox (Ctxt);

      if Get_Has_Begin (Decl) then
         Start_Hbox (Ctxt);
         Disp_Token (Ctxt, Tok_Begin);
         Close_Hbox (Ctxt);
      end if;
      if Get_Concurrent_Statement_Chain (Decl) /= Null_Iir then
         Start_Vbox (Ctxt);
         Disp_Concurrent_Statement_Chain (Ctxt, Decl);
         Close_Vbox (Ctxt);
      end if;
      Disp_End (Ctxt, Decl, Tok_Entity);
   end Disp_Entity_Declaration;

   procedure Disp_Component_Declaration
     (Ctxt : in out Ctxt_Class; Decl: Iir_Component_Declaration) is
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Component);
      Disp_Name_Of (Ctxt, Decl);
      if Get_Has_Is (Decl) then
         Disp_Token (Ctxt, Tok_Is);
      end if;
      Close_Hbox (Ctxt);

      Start_Vbox (Ctxt);
      if Get_Generic_Chain (Decl) /= Null_Iir then
         Disp_Generics (Ctxt, Decl);
      end if;
      if Get_Port_Chain (Decl) /= Null_Iir then
         Disp_Ports (Ctxt, Decl);
      end if;
      Close_Vbox (Ctxt);

      Disp_End (Ctxt, Decl, Tok_Component);
   end Disp_Component_Declaration;

   procedure Disp_Concurrent_Statement_Chain
     (Ctxt : in out Ctxt_Class; Parent : Iir)
   is
      El: Iir;
   begin
      El := Get_Concurrent_Statement_Chain (Parent);
      while El /= Null_Iir loop
         Disp_Concurrent_Statement (Ctxt, El);
         El := Get_Chain (El);
      end loop;
   end Disp_Concurrent_Statement_Chain;

   procedure Disp_Simultaneous_Statement_Chain
     (Ctxt : in out Ctxt_Class; Chain : Iir)
   is
      El: Iir;
   begin
      El := Chain;
      while El /= Null_Iir loop
         Disp_Concurrent_Statement (Ctxt, El);
         El := Get_Chain (El);
      end loop;
   end Disp_Simultaneous_Statement_Chain;

   procedure Disp_Architecture_Body
     (Ctxt : in out Ctxt_Class; Arch: Iir_Architecture_Body) is
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Architecture);
      Disp_Name_Of (Ctxt, Arch);
      Disp_Token (Ctxt, Tok_Of);
      Print (Ctxt, Get_Entity_Name (Arch));
      Close_Hbox (Ctxt);
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Is);
      Close_Hbox (Ctxt);

      Start_Vbox (Ctxt);
      Disp_Declaration_Chain (Ctxt, Arch);
      Close_Vbox (Ctxt);

      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Begin);
      Close_Hbox (Ctxt);

      Start_Vbox (Ctxt);
      Disp_Concurrent_Statement_Chain (Ctxt, Arch);
      Close_Vbox (Ctxt);

      Disp_End (Ctxt, Arch, Tok_Architecture);
   end Disp_Architecture_Body;

   procedure Disp_Signature (Ctxt : in out Ctxt_Class; Sig : Iir)
   is
      Prefix : constant Iir := Get_Signature_Prefix (Sig);
      List : constant Iir_Flist := Get_Type_Marks_List (Sig);
      El : Iir;
   begin
      if Is_Valid (Prefix) then
         --  Only in alias.
         Print (Ctxt, Prefix);
      end if;
      Disp_Token (Ctxt, Tok_Left_Bracket);
      if List /= Null_Iir_Flist then
         for I in Flist_First .. Flist_Last (List) loop
            El := Get_Nth_Element (List, I);
            if I /= 0 then
               Disp_Token (Ctxt, Tok_Comma);
            end if;
            Print (Ctxt, El);
         end loop;
      end if;
      El := Get_Return_Type_Mark (Sig);
      if El /= Null_Iir then
         Disp_Token (Ctxt, Tok_Return);
         Print (Ctxt, El);
      end if;
      Disp_Token (Ctxt, Tok_Right_Bracket);
   end Disp_Signature;

   procedure Disp_Object_Alias_Declaration
     (Ctxt : in out Ctxt_Class; Decl: Iir_Object_Alias_Declaration)
   is
      St_Ind : constant Iir := Get_Subtype_Indication (Decl);
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Alias);
      Disp_Function_Name (Ctxt, Decl);
      if St_Ind /= Null_Iir then
         Disp_Token (Ctxt, Tok_Colon);
         Disp_Subtype_Indication (Ctxt, St_Ind);
      end if;
      Disp_Token (Ctxt, Tok_Is);
      Print (Ctxt, Get_Name (Decl));
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Object_Alias_Declaration;

   procedure Disp_Non_Object_Alias_Declaration
     (Ctxt : in out Ctxt_Class; Decl: Iir_Non_Object_Alias_Declaration)
   is
      Sig : constant Iir := Get_Alias_Signature (Decl);
   begin
      if Get_Implicit_Alias_Flag (Decl) then
         if Flag_Implicit then
            OOB.Put ("-- ");
         else
            return;
         end if;
      end if;

      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Alias);
      Disp_Function_Name (Ctxt, Decl);
      Disp_Token (Ctxt, Tok_Is);
      Print (Ctxt, Get_Name (Decl));
      if Sig /= Null_Iir then
         Disp_Signature (Ctxt, Sig);
      end if;
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Non_Object_Alias_Declaration;

   procedure Disp_File_Declaration
     (Ctxt : in out Ctxt_Class; Decl: Iir_File_Declaration)
   is
      Next_Decl : Iir;
      Expr: Iir;
   begin
      Disp_Token (Ctxt, Tok_File);
      Disp_Name_Of (Ctxt, Decl);
      Next_Decl := Decl;
      while Get_Has_Identifier_List (Next_Decl) loop
         Next_Decl := Get_Chain (Next_Decl);
         Disp_Token (Ctxt, Tok_Comma);
         Disp_Name_Of (Ctxt, Next_Decl);
      end loop;
      Disp_Token (Ctxt, Tok_Colon);
      Disp_Subtype_Indication (Ctxt, Or_Else (Get_Subtype_Indication (Decl),
                                              Get_Type (Decl)));
      if Vhdl_Std = Vhdl_87 then
         Disp_Token (Ctxt, Tok_Is);
         if Get_Has_Mode (Decl) then
            Disp_Mode (Ctxt, Get_Mode (Decl));
         end if;
         Print (Ctxt, Get_File_Logical_Name (Decl));
      else
         Expr := Get_File_Open_Kind (Decl);
         if Expr /= Null_Iir then
            Disp_Token (Ctxt, Tok_Open);
            Print (Ctxt, Expr);
         end if;
         Expr := Get_File_Logical_Name (Decl);
         if Expr /= Null_Iir then
            Disp_Token (Ctxt, Tok_Is);
            Print (Ctxt, Expr);
         end if;
      end if;
      Disp_Token (Ctxt, Tok_Semi_Colon);
   end Disp_File_Declaration;

   procedure Disp_Branch_Quantity_Declaration
     (Ctxt : in out Ctxt_Class; Head : Iir)
   is
      Term : Iir;
      Decl : Iir;
      First_Decl : Iir;
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Quantity);

      Decl := Head;
      if Get_Kind (Decl) = Iir_Kind_Across_Quantity_Declaration then
         loop
            Disp_Name_Of (Ctxt, Decl);
            if not Get_Has_Identifier_List (Decl) then
               Decl := Null_Iir;
               exit;
            end if;
            Decl := Get_Chain (Decl);
            exit when Get_Kind (Decl) /= Iir_Kind_Across_Quantity_Declaration;
            Disp_Token (Ctxt, Tok_Comma);
         end loop;

         Disp_Tolerance_Opt (Ctxt, Head);
         Disp_Default_Value_Opt (Ctxt, Head);
         Disp_Token (Ctxt, Tok_Across);
      end if;

      if Decl /= Null_Iir then
         pragma Assert
           (Get_Kind (Decl) = Iir_Kind_Through_Quantity_Declaration);

         First_Decl := Decl;
         loop
            Disp_Name_Of (Ctxt, Decl);
            if not Get_Has_Identifier_List (Decl) then
               Decl := Null_Iir;
               exit;
            end if;
            Decl := Get_Chain (Decl);
            exit when Get_Kind (Decl) /= Iir_Kind_Through_Quantity_Declaration;
            Disp_Token (Ctxt, Tok_Comma);
         end loop;

         Disp_Tolerance_Opt (Ctxt, First_Decl);
         Disp_Default_Value_Opt (Ctxt, First_Decl);
         Disp_Token (Ctxt, Tok_Through);
      end if;

      Print (Ctxt, Get_Plus_Terminal_Name (Head));
      Term := Get_Minus_Terminal_Name (Head);
      if Term /= Null_Iir then
         Disp_Token (Ctxt, Tok_To);
         Print (Ctxt, Term);
      end if;

      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Branch_Quantity_Declaration;

   procedure Disp_Terminal_Declaration (Ctxt : in out Ctxt_Class; Decl: Iir)
   is
      Ndecl : Iir;
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Terminal);
      Disp_Name_Of (Ctxt, Decl);
      Ndecl := Decl;
      while Get_Has_Identifier_List (Ndecl) loop
         Disp_Token (Ctxt, Tok_Comma);
         Ndecl := Get_Chain (Ndecl);
         Disp_Name_Of (Ctxt, Ndecl);
      end loop;
      Disp_Token (Ctxt, Tok_Colon);
      Disp_Subnature_Indication (Ctxt, Get_Subnature_Indication (Decl));
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Terminal_Declaration;

   procedure Disp_Object_Declaration (Ctxt : in out Ctxt_Class; Decl: Iir)
   is
      Next_Decl : Iir;
   begin
      Start_Node (Ctxt, Decl);

      Start_Hbox (Ctxt);
      case Get_Kind (Decl) is
         when Iir_Kind_Variable_Declaration =>
            if Get_Shared_Flag (Decl) then
               Disp_Token (Ctxt, Tok_Shared);
            end if;
            Disp_Token (Ctxt, Tok_Variable);
         when Iir_Kind_Constant_Declaration =>
            Disp_Token (Ctxt, Tok_Constant);
         when Iir_Kind_Signal_Declaration =>
            Disp_Token (Ctxt, Tok_Signal);
         when Iir_Kind_File_Declaration =>
            Disp_File_Declaration (Ctxt, Decl);
            Close_Hbox (Ctxt);
            return;
         when Iir_Kind_Free_Quantity_Declaration
           | Iir_Kinds_Source_Quantity_Declaration =>
            Disp_Token (Ctxt, Tok_Quantity);
         when others =>
            raise Internal_Error;
      end case;
      Disp_Name_Of (Ctxt, Decl);
      Next_Decl := Decl;
      while Get_Has_Identifier_List (Next_Decl) loop
         Next_Decl := Get_Chain (Next_Decl);
         Disp_Token (Ctxt, Tok_Comma);
         Disp_Name_Of (Ctxt, Next_Decl);
      end loop;
      Valign (Ctxt, Valign_Colon);
      Disp_Token (Ctxt, Tok_Colon);
      Valign (Ctxt, Valign_Typemark);
      Disp_Subtype_Indication (Ctxt, Get_Subtype_Indication (Decl));
      if Get_Kind (Decl) = Iir_Kind_Signal_Declaration then
         Disp_Signal_Kind (Ctxt, Decl);
      end if;

      case Get_Kind (Decl) is
         when Iir_Kind_Spectrum_Quantity_Declaration =>
            Disp_Token (Ctxt, Tok_Spectrum);
            Print (Ctxt, Get_Magnitude_Expression (Decl));
            Disp_Token (Ctxt, Tok_Comma);
            Print (Ctxt, Get_Phase_Expression (Decl));
         when Iir_Kind_Noise_Quantity_Declaration =>
            Disp_Token (Ctxt, Tok_Noise);
            Print (Ctxt, Get_Power_Expression (Decl));
         when others =>
            Disp_Default_Value_Opt (Ctxt, Decl);
      end case;
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Object_Declaration;

   procedure Disp_Pure (Ctxt : in out Ctxt_Class; Subprg : Iir) is
   begin
      if Get_Pure_Flag (Subprg) then
         Disp_Token (Ctxt, Tok_Pure);
      else
         Disp_Token (Ctxt, Tok_Impure);
      end if;
   end Disp_Pure;

   procedure Disp_Subprogram_Declaration
     (Ctxt : in out Ctxt_Class; Subprg: Iir; Implicit : Boolean := False)
   is
      Inter : Iir;
      Default : Iir;
   begin
      if Implicit then
         OOB.Put ("-- ");
      end if;

      case Get_Kind (Subprg) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Interface_Function_Declaration =>
            if Get_Has_Pure (Subprg) then
               Disp_Pure (Ctxt, Subprg);
            end if;
            Disp_Token (Ctxt, Tok_Function);
         when Iir_Kind_Procedure_Declaration
           | Iir_Kind_Interface_Procedure_Declaration =>
            Disp_Token (Ctxt, Tok_Procedure);
         when others =>
            raise Internal_Error;
      end case;

      Disp_Function_Name (Ctxt, Subprg);

      if Get_Has_Parameter (Subprg) then
         Disp_Token (Ctxt, Tok_Parameter);
      end if;

      Inter := Get_Interface_Declaration_Chain (Subprg);
      Disp_Interface_Chain (Ctxt, Inter, True);

      case Get_Kind (Subprg) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Interface_Function_Declaration =>
            Disp_Token (Ctxt, Tok_Return);
            Disp_Subtype_Indication
              (Ctxt, Or_Else (Get_Return_Type_Mark (Subprg),
                              Get_Return_Type (Subprg)));
         when Iir_Kind_Procedure_Declaration
           | Iir_Kind_Interface_Procedure_Declaration =>
            null;
         when others =>
            raise Internal_Error;
      end case;

      if Get_Kind (Subprg) in Iir_Kinds_Interface_Subprogram_Declaration then
         Default := Get_Default_Subprogram (Subprg);
         if Default /= Null_Iir then
            Disp_Token (Ctxt, Tok_Is);
            if Get_Kind (Default) = Iir_Kind_Reference_Name then
               Disp_Token (Ctxt, Tok_Box);
            else
               Print (Ctxt, Default);
            end if;
         end if;
      end if;
   end Disp_Subprogram_Declaration;

   procedure Disp_Subprogram_Body (Ctxt : in out Ctxt_Class; Subprg : Iir) is
   begin
      Start_Vbox (Ctxt);
      Disp_Declaration_Chain (Ctxt, Subprg);
      Close_Vbox (Ctxt);
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Begin);
      Close_Hbox (Ctxt);
      Start_Vbox (Ctxt);
      Disp_Sequential_Statements
        (Ctxt, Get_Sequential_Statement_Chain (Subprg));
      Close_Vbox (Ctxt);
      if Get_Kind (Subprg) = Iir_Kind_Function_Body then
         Disp_End (Ctxt, Subprg, Tok_Function);
      else
         Disp_End (Ctxt, Subprg, Tok_Procedure);
      end if;
   end Disp_Subprogram_Body;

   procedure Disp_Instantiation_List
     (Ctxt : in out Ctxt_Class; Insts: Iir_Flist)
   is
      El : Iir;
   begin
      case Insts is
         when Iir_Flist_All =>
            Disp_Token (Ctxt, Tok_All);
         when Iir_Flist_Others =>
            Disp_Token (Ctxt, Tok_Others);
         when others =>
            for I in Flist_First .. Flist_Last (Insts) loop
               El := Get_Nth_Element (Insts, I);
               if I /= Flist_First then
                  Disp_Token (Ctxt, Tok_Comma);
               end if;
               Print (Ctxt, El);
            end loop;
      end case;
   end Disp_Instantiation_List;

   procedure Disp_Configuration_Specification
     (Ctxt : in out Ctxt_Class; Spec : Iir_Configuration_Specification) is
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_For);
      Disp_Instantiation_List (Ctxt, Get_Instantiation_List (Spec));
      Disp_Token (Ctxt, Tok_Colon);
      Print (Ctxt, Get_Component_Name (Spec));
      Disp_Binding_Indication (Ctxt, Get_Binding_Indication (Spec));
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Configuration_Specification;

   procedure Disp_Disconnection_Specification
     (Ctxt : in out Ctxt_Class; Dis : Iir_Disconnection_Specification) is
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Disconnect);
      Disp_Instantiation_List (Ctxt, Get_Signal_List (Dis));
      Disp_Token (Ctxt, Tok_Colon);
      Print (Ctxt, Get_Type_Mark (Dis));
      Disp_Token (Ctxt, Tok_After);
      Print (Ctxt, Get_Expression (Dis));
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Disconnection_Specification;

   procedure Disp_Step_Limit_Specification
     (Ctxt : in out Ctxt_Class; Limit : Iir) is
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Limit);
      Disp_Instantiation_List (Ctxt, Get_Quantity_List (Limit));
      Disp_Token (Ctxt, Tok_Colon);
      Print (Ctxt, Get_Type_Mark (Limit));
      Disp_Token (Ctxt, Tok_With);
      Print (Ctxt, Get_Expression (Limit));
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Step_Limit_Specification;

   procedure Disp_Attribute_Declaration
     (Ctxt : in out Ctxt_Class; Attr : Iir_Attribute_Declaration) is
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Attribute);
      Disp_Identifier (Ctxt, Attr);
      Disp_Token (Ctxt, Tok_Colon);
      Print (Ctxt, Get_Type_Mark (Attr));
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Attribute_Declaration;

   procedure Disp_Attribute_Value (Ctxt : in out Ctxt_Class; Attr : Iir) is
   begin
      Disp_Name_Of (Ctxt, Get_Designated_Entity (Attr));
      Disp_Token (Ctxt, Tok_Tick);
      Disp_Identifier
        (Ctxt, Get_Attribute_Designator (Get_Attribute_Specification (Attr)));
   end Disp_Attribute_Value;

   procedure Disp_Attribute_Name (Ctxt : in out Ctxt_Class; Attr : Iir)
   is
      Sig : constant Iir := Get_Attribute_Signature (Attr);
   begin
      Print (Ctxt, Get_Prefix (Attr));
      if Sig /= Null_Iir then
         Disp_Signature (Ctxt, Sig);
      end if;
      Disp_Token (Ctxt, Tok_Tick);
      Disp_Ident (Ctxt, Get_Identifier (Attr));
   end Disp_Attribute_Name;

   procedure Disp_Entity_Kind (Ctxt : in out Ctxt_Class; Tok : Token_Type) is
   begin
      Disp_Token (Ctxt, Tok);
   end Disp_Entity_Kind;

   procedure Disp_Entity_Name_List (Ctxt : in out Ctxt_Class; List : Iir_Flist)
   is
      El : Iir;
   begin
      case List is
         when Iir_Flist_All =>
            Disp_Token (Ctxt, Tok_All);
         when Iir_Flist_Others =>
            Disp_Token (Ctxt, Tok_Others);
         when others =>
            for I in Flist_First .. Flist_Last (List) loop
               El := Get_Nth_Element (List, I);
               if I /= Flist_First then
                  Disp_Token (Ctxt, Tok_Comma);
               end if;
               Print (Ctxt, El);
            end loop;
      end case;
   end Disp_Entity_Name_List;

   procedure Disp_Attribute_Specification
     (Ctxt : in out Ctxt_Class; Attr : Iir_Attribute_Specification) is
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Attribute);
      Disp_Identifier (Ctxt, Get_Attribute_Designator (Attr));
      Disp_Token (Ctxt, Tok_Of);
      Disp_Entity_Name_List (Ctxt, Get_Entity_Name_List (Attr));
      Disp_Token (Ctxt, Tok_Colon);
      Disp_Entity_Kind (Ctxt, Get_Entity_Class (Attr));
      Disp_Token (Ctxt, Tok_Is);
      Print (Ctxt, Get_Expression (Attr));
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Attribute_Specification;

   procedure Disp_Protected_Type_Body
     (Ctxt : in out Ctxt_Class; Bod : Iir_Protected_Type_Body) is
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Type);
      Disp_Identifier (Ctxt, Bod);
      Disp_Token (Ctxt, Tok_Is);
      Disp_Token (Ctxt, Tok_Protected, Tok_Body);
      Close_Hbox (Ctxt);

      Start_Vbox (Ctxt);
      Disp_Declaration_Chain (Ctxt, Bod);
      Close_Vbox (Ctxt);

      Disp_End (Ctxt, Bod, Tok_Protected, Tok_Body);
   end Disp_Protected_Type_Body;

   procedure Disp_Group_Template_Declaration
     (Ctxt : in out Ctxt_Class; Decl : Iir)
   is
      Ent : Iir;
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Group);
      Disp_Identifier (Ctxt, Decl);
      Disp_Token (Ctxt, Tok_Is, Tok_Left_Paren);
      Ent := Get_Entity_Class_Entry_Chain (Decl);
      loop
         Disp_Entity_Kind (Ctxt, Get_Entity_Class (Ent));
         Ent := Get_Chain (Ent);
         exit when Ent = Null_Iir;
         if Get_Entity_Class (Ent) = Tok_Box then
            Disp_Token (Ctxt, Tok_Box);
            exit;
         else
            Disp_Token (Ctxt, Tok_Comma);
         end if;
      end loop;
      Disp_Token (Ctxt, Tok_Right_Paren, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Group_Template_Declaration;

   procedure Disp_Group_Declaration (Ctxt : in out Ctxt_Class; Decl : Iir)
   is
      List : Iir_Flist;
      El : Iir;
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Group);
      Disp_Identifier (Ctxt, Decl);
      Disp_Token (Ctxt, Tok_Colon);
      Print (Ctxt, Get_Group_Template_Name (Decl));
      Disp_Token (Ctxt, Tok_Left_Paren);
      List := Get_Group_Constituent_List (Decl);
      for I in Flist_First .. Flist_Last (List) loop
         El := Get_Nth_Element (List, I);
         if I /= 0 then
            Disp_Token (Ctxt, Tok_Comma);
         end if;
         Disp_Name_Of (Ctxt, El);
      end loop;
      Disp_Token (Ctxt, Tok_Right_Paren, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Group_Declaration;

   procedure Print_Expr (Ctxt : in out Ctxt_Class;
                         N : PSL_Node;
                         Parent_Prio : Priority := Prio_Lowest)
   is
      Prio : Priority;
   begin
      if N = Null_PSL_Node then
         OOB.Put (".");
         return;
      end if;
      Prio := PSL.Prints.Get_Priority (N);
      if Prio < Parent_Prio then
         Disp_Token (Ctxt, Tok_Left_Paren);
      end if;
      case Get_Kind (N) is
         when N_Number =>
            declare
               Str : constant String := Uns32'Image (Get_Value (N));
            begin
               Start_Lit (Ctxt, Tok_Integer);
               Disp_Str (Ctxt, Str);
               Close_Lit (Ctxt);
            end;
         when N_Inf =>
            Start_Lit (Ctxt, Tok_Identifier);
            Disp_Str (Ctxt, "INF");
            Close_Lit (Ctxt);
         when N_Name_Decl =>
            Disp_Ident (Ctxt, Get_Identifier (N));
         when N_HDL_Expr
           | N_HDL_Bool =>
            Print (Ctxt, Vhdl_Node (PSL.Nodes.Get_HDL_Node (N)));
            --  FIXME: this is true only when using the scanner.
            --  Print_Expr (Node (Get_HDL_Node (N)));
         when N_True =>
            Start_Lit (Ctxt, Tok_Identifier);
            Disp_Str (Ctxt, "TRUE");
            Close_Lit (Ctxt);
         when N_False =>
            Start_Lit (Ctxt, Tok_Identifier);
            Disp_Str (Ctxt, "FALSE");
            Close_Lit (Ctxt);
         when N_EOS =>
            Start_Lit (Ctxt, Tok_Identifier);
            Disp_Str (Ctxt, "EOS");
            Close_Lit (Ctxt);
         when N_Not_Bool =>
            Disp_Token (Ctxt, Tok_Not);
--            Disp_Token (Ctxt, Tok_Left_Paren);
            Print_Expr (Ctxt, Get_Boolean (N), Prio);
--            Disp_Token (Ctxt, Tok_Right_Paren);
         when N_And_Bool =>
            Disp_Token (Ctxt, Tok_Left_Paren);
            Print_Expr (Ctxt, Get_Left (N), Prio);
            Disp_Token (Ctxt, Tok_And);
            Print_Expr (Ctxt, Get_Right (N), Prio);
            Disp_Token (Ctxt, Tok_Right_Paren);
         when N_Or_Bool =>
            Disp_Token (Ctxt, Tok_Left_Paren);
            Print_Expr (Ctxt, Get_Left (N), Prio);
            Disp_Token (Ctxt, Tok_Or);
            Print_Expr (Ctxt, Get_Right (N), Prio);
            Disp_Token (Ctxt, Tok_Right_Paren);
         when N_Imp_Bool =>
            Print_Expr (Ctxt, Get_Left (N), Prio);
            Disp_Token (Ctxt, Tok_Minus_Greater);
            Print_Expr (Ctxt, Get_Right (N), Prio);
         when others =>
            PSL.Errors.Error_Kind ("print_expr", N);
      end case;
      if Prio < Parent_Prio then
         Disp_Token (Ctxt, Tok_Right_Paren);
      end if;
   end Print_Expr;

   procedure Print_Count (Ctxt : in out Ctxt_Class; N : PSL_Node)
   is
      B : PSL_Node;
   begin
      B := Get_Low_Bound (N);
      if B = Null_PSL_Node then
         return;
      end if;
      Print_Expr (Ctxt, B);
      B := Get_High_Bound (N);
      if B = Null_PSL_Node then
         return;
      end if;
      Disp_Token (Ctxt, Tok_Colon);
      Print_Expr (Ctxt, B);
   end Print_Count;

   procedure Print_Binary_Sequence (Ctxt : in out Ctxt_Class;
                                    Tok : Token_Type;
                                    N : PSL_Node;
                                    Prio : Priority) is
   begin
      Print_Sequence (Ctxt, Get_Left (N), Prio);
      Disp_Token (Ctxt, Tok);
      Print_Sequence (Ctxt, Get_Right (N), Prio);
   end Print_Binary_Sequence;

   procedure Print_Seq_Repeat_Sere (Ctxt : in out Ctxt_Class; N : PSL_Node)
   is
      S : constant PSL_Node := Get_Sequence (N);
   begin
      if S /= Null_PSL_Node then
         Print_Sequence (Ctxt, S, Prio_SERE_Repeat);
      end if;
      Disp_Token (Ctxt, Tok_Brack_Star);
      Print_Count (Ctxt, N);
      Disp_Token (Ctxt, Tok_Right_Bracket);
   end Print_Seq_Repeat_Sere;

   procedure Print_Bool_Repeat_Sere
     (Ctxt : in out Ctxt_Class; Tok : Token_Type; N : PSL_Node) is
   begin
      Print_Expr (Ctxt, Get_Boolean (N));
      Disp_Token (Ctxt, Tok);
      Print_Count (Ctxt, N);
      Disp_Token (Ctxt, Tok_Right_Bracket);
   end Print_Bool_Repeat_Sere;

   procedure Print_PSL_Instance (Ctxt : in out Ctxt_Class; Inst : PSL_Node)
   is
      Decl : constant PSL_Node := Get_Declaration (Inst);
      Assoc : PSL_Node;
      Actual, Formal : PSL_Node;
   begin
      Disp_Ident (Ctxt, Get_Identifier (Decl));
      Assoc := Get_Association_Chain (Inst);
      if Assoc = Null_PSL_Node then
         return;
      end if;
      Disp_Token (Ctxt, Tok_Left_Paren);
      loop
         Actual := Get_Actual (Assoc);
         Formal := Get_Formal (Assoc);
         case Get_Kind (Formal) is
            when N_Boolean_Parameter =>
               Print_Expr (Ctxt, Actual);
            when others =>
               PSL.Errors.Error_Kind ("print_psl_instance", Formal);
         end case;
         Assoc := Get_Chain (Assoc);
         exit when Assoc = Null_PSL_Node;
         Disp_Token (Ctxt, Tok_Comma);
      end loop;
      Disp_Token (Ctxt, Tok_Right_Paren);
   end Print_PSL_Instance;

   procedure Print_Sequence (Ctxt : in out Ctxt_Class;
                             Seq : PSL_Node;
                             Parent_Prio : Priority := Prio_Lowest)
   is
      Prio : constant Priority := PSL.Prints.Get_Priority (Seq);
      Add_Paren : constant Boolean := Prio < Parent_Prio
        or else Parent_Prio <= Prio_FL_Paren;
   begin
      if Get_Kind (Seq) = N_Sequence_Instance then
         Print_PSL_Instance (Ctxt, Seq);
         return;
      end if;

      if Add_Paren then
         Disp_Token (Ctxt, Tok_Left_Curly);
      end if;
      case Get_Kind (Seq) is
         when N_Braced_SERE =>
            Disp_Token (Ctxt, Tok_Left_Curly);
            Print_Sequence (Ctxt, Get_SERE (Seq), Prio_Lowest);
            Disp_Token (Ctxt, Tok_Right_Curly);
         when N_Concat_SERE =>
            Print_Binary_Sequence (Ctxt, Tok_Semi_Colon, Seq, Prio);
         when N_Fusion_SERE =>
            Print_Binary_Sequence (Ctxt, Tok_Colon, Seq, Prio);
         when N_Within_SERE =>
            Print_Binary_Sequence (Ctxt, Tok_Within, Seq, Prio);
         when N_Match_And_Seq =>
            Print_Binary_Sequence (Ctxt, Tok_And_And, Seq, Prio);
         when N_Or_Seq =>
            Print_Binary_Sequence (Ctxt, Tok_Bar, Seq, Prio);
         when N_And_Seq =>
            Print_Binary_Sequence (Ctxt, Tok_Ampersand, Seq, Prio);
         when N_Star_Repeat_Seq =>
            Print_Seq_Repeat_Sere (Ctxt, Seq);
         when N_Goto_Repeat_Seq =>
            Print_Bool_Repeat_Sere (Ctxt, Tok_Brack_Arrow, Seq);
         when N_Equal_Repeat_Seq =>
            Print_Bool_Repeat_Sere (Ctxt, Tok_Brack_Equal, Seq);
         when N_Plus_Repeat_Seq =>
            Print_Sequence (Ctxt, Get_Sequence (Seq), Prio);
            Disp_Token (Ctxt, Tok_Brack_Plus_Brack);
         when N_Booleans
           | N_Name_Decl =>
            Print_Expr (Ctxt, Seq);
         when N_Boolean_Parameter =>
            Disp_Ident (Ctxt, Get_Identifier (Seq));
         when others =>
            PSL.Errors.Error_Kind ("print_sequence", Seq);
      end case;
      if Add_Paren then
         Disp_Token (Ctxt, Tok_Right_Curly);
      end if;
   end Print_Sequence;

   procedure Print_Binary_Property (Ctxt : in out Ctxt_Class;
                                    Tok : Token_Type;
                                    N : PSL_Node;
                                    Prio : Priority) is
   begin
      Print_Property (Ctxt, Get_Left (N), Prio);
      Disp_Token (Ctxt, Tok);
      Print_Property (Ctxt, Get_Right (N), Prio);
   end Print_Binary_Property;

   procedure Print_Binary_Property_SI (Ctxt : in out Ctxt_Class;
                                       Op, Op_Em, Op_Un, Op_Em_Un : Token_Type;
                                       N : PSL_Node;
                                       Prio : Priority) is
   begin
      Print_Property (Ctxt, Get_Left (N), Prio);
      if Get_Strong_Flag (N) then
         if Get_Inclusive_Flag (N) then
            Disp_Token (Ctxt, Op_Em_Un);
         else
            Disp_Token (Ctxt, Op_Em);
         end if;
      else
         if Get_Inclusive_Flag (N) then
            Disp_Token (Ctxt, Op_Un);
         else
            Disp_Token (Ctxt, Op);
         end if;
      end if;
      Print_Property (Ctxt, Get_Right (N), Prio);
   end Print_Binary_Property_SI;

   procedure Print_Range_Property
     (Ctxt : in out Ctxt_Class; Tok : Token_Type; N : PSL_Node) is
   begin
      Disp_Token (Ctxt, Tok);
      Disp_Token (Ctxt, Tok_Left_Bracket);
      Print_Count (Ctxt, N);
      Disp_Token (Ctxt, Tok_Right_Bracket);
      Disp_Token (Ctxt, Tok_Left_Paren);
      Print_Property (Ctxt, Get_Property (N), Prio_FL_Paren);
      Disp_Token (Ctxt, Tok_Right_Paren);
   end Print_Range_Property;

   procedure Print_Boolean_Range_Property
     (Ctxt : in out Ctxt_Class; Tok : Token_Type; N : PSL_Node) is
   begin
      Disp_Token (Ctxt, Tok);
      Disp_Token (Ctxt, Tok_Left_Paren);
      Print_Expr (Ctxt, Get_Boolean (N));
      Disp_Token (Ctxt, Tok_Right_Paren, Tok_Left_Bracket);
      Print_Count (Ctxt, N);
      Disp_Token (Ctxt, Tok_Right_Bracket, Tok_Left_Paren);
      Print_Property (Ctxt, Get_Property (N), Prio_FL_Paren);
      Disp_Token (Ctxt, Tok_Right_Paren);
   end Print_Boolean_Range_Property;

   procedure Print_Abort_Property (Ctxt : in out Ctxt_Class;
                                   Tok : Token_Type;
                                   N : PSL_Node;
                                   Prio : Priority) is
   begin
      Print_Property (Ctxt, Get_Property (N), Prio);
      Disp_Token (Ctxt, Tok);
      Print_Expr (Ctxt, Get_Boolean (N));
   end Print_Abort_Property;

   procedure Print_Property (Ctxt : in out Ctxt_Class;
                             Prop : PSL_Node;
                             Parent_Prio : Priority := Prio_Lowest)
   is
      Prio : constant Priority := PSL.Prints.Get_Priority (Prop);
   begin
      if Prio < Parent_Prio then
         Disp_Token (Ctxt, Tok_Left_Paren);
      end if;
      case Get_Kind (Prop) is
         when N_Never =>
            Disp_Token (Ctxt, Tok_Never);
            Print_Property (Ctxt, Get_Property (Prop), Prio);
         when N_Always =>
            Disp_Token (Ctxt, Tok_Always, Tok_Left_Paren);
            Print_Property (Ctxt, Get_Property (Prop), Prio);
            Disp_Token (Ctxt, Tok_Right_Paren);
         when N_Eventually =>
            Disp_Token (Ctxt, Tok_Eventually_Em, Tok_Left_Paren);
            Print_Property (Ctxt, Get_Property (Prop), Prio);
            Disp_Token (Ctxt, Tok_Right_Paren);
         when N_Strong =>
            Print_Property (Ctxt, Get_Property (Prop), Prio);
            Disp_Token (Ctxt, Tok_Exclam_Mark);
         when N_Next =>
            Disp_Token (Ctxt, Tok_Next);
--              if Get_Strong_Flag (Prop) then
--                 Put ('!');
--              end if;
            Disp_Token (Ctxt, Tok_Left_Paren);
            Print_Property (Ctxt, Get_Property (Prop), Prio);
            Disp_Token (Ctxt, Tok_Right_Paren);
         when N_Next_A =>
            Print_Range_Property (Ctxt, Tok_Next_A, Prop);
         when N_Next_E =>
            Print_Range_Property (Ctxt, Tok_Next_E, Prop);
         when N_Next_Event =>
            Disp_Token (Ctxt, Tok_Next_Event, Tok_Left_Paren);
            Print_Expr (Ctxt, Get_Boolean (Prop));
            Disp_Token (Ctxt, Tok_Right_Paren, Tok_Left_Paren);
            Print_Property (Ctxt, Get_Property (Prop), Prio);
            Disp_Token (Ctxt, Tok_Right_Paren);
         when N_Next_Event_A =>
            Print_Boolean_Range_Property (Ctxt, Tok_Next_Event_A, Prop);
         when N_Next_Event_E =>
            Print_Boolean_Range_Property (Ctxt, Tok_Next_Event_E, Prop);
         when N_Until =>
            Print_Binary_Property_SI
              (Ctxt,
               Tok_Until, Tok_Until_Em, Tok_Until_Un, Tok_Until_Em_Un,
               Prop, Prio);
         when N_Abort =>
            Print_Abort_Property (Ctxt, Tok_Abort, Prop, Prio);
         when N_Sync_Abort =>
            Print_Abort_Property (Ctxt, Tok_Sync_Abort, Prop, Prio);
         when N_Async_Abort =>
            Print_Abort_Property (Ctxt, Tok_Async_Abort, Prop, Prio);
         when N_Before =>
            Print_Binary_Property_SI
              (Ctxt,
               Tok_Before, Tok_Before_Em, Tok_Before_Un, Tok_Before_Em_Un,
               Prop, Prio);
         when N_Or_Prop =>
            Print_Binary_Property (Ctxt, Tok_Or, Prop, Prio);
         when N_And_Prop =>
            Print_Binary_Property (Ctxt, Tok_And, Prop, Prio);
         when N_Paren_Prop =>
            Disp_Token (Ctxt, Tok_Left_Paren);
            Print_Property (Ctxt, Get_Property (Prop), Prio);
            Disp_Token (Ctxt, Tok_Right_Paren);
         when N_Imp_Seq =>
            Print_Property (Ctxt, Get_Sequence (Prop), Prio);
            Disp_Token (Ctxt, Tok_Bar_Double_Arrow);
            Print_Property (Ctxt, Get_Property (Prop), Prio);
         when N_Overlap_Imp_Seq =>
            Print_Property (Ctxt, Get_Sequence (Prop), Prio);
            Disp_Token (Ctxt, Tok_Bar_Arrow);
            Print_Property (Ctxt, Get_Property (Prop), Prio);
         when N_Log_Imp_Prop =>
            Print_Binary_Property (Ctxt, Tok_Minus_Greater, Prop, Prio);
         when N_Booleans
           | N_Name_Decl =>
            Print_Expr (Ctxt, Prop);
         when N_Sequences =>
            Print_Sequence (Ctxt, Prop, Parent_Prio);
         when N_Property_Instance =>
            Disp_Ident (Ctxt, Get_Identifier (Get_Declaration (Prop)));
         when N_EOS =>
            Start_Lit (Ctxt, Tok_Identifier);
            Disp_Str (Ctxt, "EOS");
            Close_Lit (Ctxt);
         when others =>
            PSL.Errors.Error_Kind ("print_property", Prop);
      end case;
      if Prio < Parent_Prio then
         Disp_Token (Ctxt, Tok_Right_Paren);
      end if;
   end Print_Property;

   procedure Disp_Psl_Expression
     (Ctxt : in out Ctxt_Class; Expr : PSL_Node)  is
   begin
      Print_Property (Ctxt, Expr);
   end Disp_Psl_Expression;

   procedure Disp_Psl_Default_Clock (Ctxt : in out Ctxt_Class; Stmt : Iir) is
   begin
      Start_Hbox (Ctxt);
      if Vhdl_Std < Vhdl_08 then
         OOB.Put ("--psl ");
      end if;
      Disp_Token (Ctxt, Tok_Default, Tok_Psl_Clock);
      Disp_Token (Ctxt, Tok_Is);
      Disp_Psl_Expression (Ctxt, Get_Psl_Boolean (Stmt));
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Psl_Default_Clock;

   procedure Disp_Psl_Prev (Ctxt : in out Ctxt_Class; Call : Iir)
   is
      Expr : Iir;
   begin
      Disp_Token (Ctxt, Tok_Prev);
      Disp_Token (Ctxt, Tok_Left_Paren);
      Print (Ctxt, Get_Expression (Call));
      Expr := Get_Count_Expression (Call);
      if Expr /= Null_Iir then
         Disp_Token (Ctxt, Tok_Comma);
         Print (Ctxt, Expr);

         Expr := Get_Clock_Expression (Call);
         if Expr /= Null_Iir then
            Disp_Token (Ctxt, Tok_Comma);
            Print (Ctxt, Expr);
         end if;
      end if;
      Disp_Token (Ctxt, Tok_Right_Paren);
   end Disp_Psl_Prev;

   procedure Disp_Psl_Stable (Ctxt : in out Ctxt_Class; Call : Iir)
   is
      Expr : Iir;
   begin
      Disp_Token (Ctxt, Tok_Stable);
      Disp_Token (Ctxt, Tok_Left_Paren);
      Print (Ctxt, Get_Expression (Call));
      Expr := Get_Clock_Expression (Call);
      if Expr /= Null_Iir then
         Disp_Token (Ctxt, Tok_Comma);
         Print (Ctxt, Expr);
      end if;
      Disp_Token (Ctxt, Tok_Right_Paren);
   end Disp_Psl_Stable;

   procedure Disp_Psl_Rose (Ctxt : in out Ctxt_Class; Call : Iir)
   is
      Expr : Iir;
   begin
      Disp_Token (Ctxt, Tok_Rose);
      Disp_Token (Ctxt, Tok_Left_Paren);
      Print (Ctxt, Get_Expression (Call));
      Expr := Get_Clock_Expression (Call);
      if Expr /= Null_Iir then
         Disp_Token (Ctxt, Tok_Comma);
         Print (Ctxt, Expr);
      end if;
      Disp_Token (Ctxt, Tok_Right_Paren);
   end Disp_Psl_Rose;

   procedure Disp_Psl_Fell (Ctxt : in out Ctxt_Class; Call : Iir)
   is
      Expr : Iir;
   begin
      Disp_Token (Ctxt, Tok_Fell);
      Disp_Token (Ctxt, Tok_Left_Paren);
      Print (Ctxt, Get_Expression (Call));
      Expr := Get_Clock_Expression (Call);
      if Expr /= Null_Iir then
         Disp_Token (Ctxt, Tok_Comma);
         Print (Ctxt, Expr);
      end if;
      Disp_Token (Ctxt, Tok_Right_Paren);
   end Disp_Psl_Fell;

   procedure Disp_Psl_Onehot (Ctxt : in out Ctxt_Class; Call : Iir) is
   begin
      Disp_Token (Ctxt, Tok_Onehot);
      Disp_Token (Ctxt, Tok_Left_Paren);
      Print (Ctxt, Get_Expression (Call));
      Disp_Token (Ctxt, Tok_Right_Paren);
   end Disp_Psl_Onehot;

   procedure Disp_Psl_Onehot0 (Ctxt : in out Ctxt_Class; Call : Iir) is
   begin
      Disp_Token (Ctxt, Tok_Onehot0);
      Disp_Token (Ctxt, Tok_Left_Paren);
      Print (Ctxt, Get_Expression (Call));
      Disp_Token (Ctxt, Tok_Right_Paren);
   end Disp_Psl_Onehot0;

   procedure Disp_Psl_Parameter_List
     (Ctxt : in out Ctxt_Class; Decl : PSL_Node)
   is
      Param : PSL_Node;
   begin
      Param := Get_Parameter_List (Decl);
      if Param = Null_PSL_Node then
         return;
      end if;
      Disp_Token (Ctxt, Tok_Left_Paren);
      loop
         case Get_Kind (Param) is
            when N_Boolean_Parameter =>
               Disp_Ident (Ctxt, Name_Boolean);
            when N_Property_Parameter =>
               Disp_Token (Ctxt, Tok_Property);
            when N_Sequence_Parameter =>
               Disp_Token (Ctxt, Tok_Sequence);
            when others =>
               PSL.Errors.Error_Kind ("disp_psl_parameter_list", Param);
         end case;
         loop
            Disp_Ident (Ctxt, Get_Identifier (Param));
            exit when not Get_Has_Identifier_List (Param);
            Disp_Token (Ctxt, Tok_Comma);
            Param := Get_Chain (Param);
         end loop;
         Param := Get_Chain (Param);
         exit when Param = Null_PSL_Node;
         Disp_Token (Ctxt, Tok_Semi_Colon);
      end loop;
      Disp_Token (Ctxt, Tok_Right_Paren);
   end Disp_Psl_Parameter_List;

   procedure Disp_Psl_Declaration (Ctxt : in out Ctxt_Class; Stmt : Iir)
   is
      Decl : constant PSL_Node := Get_Psl_Declaration (Stmt);
   begin
      Start_Hbox (Ctxt);
      if Vhdl_Std < Vhdl_08 then
         OOB.Put ("--psl ");
      end if;
      case Get_Kind (Decl) is
         when N_Property_Declaration =>
            Disp_Token (Ctxt, Tok_Property);
            Disp_Ident (Ctxt, Get_Identifier (Decl));
            Disp_Psl_Parameter_List (Ctxt, Decl);
            Disp_Token (Ctxt, Tok_Is);
            Disp_Psl_Expression (Ctxt, Get_Property (Decl));
            Disp_Token (Ctxt, Tok_Semi_Colon);
         when N_Sequence_Declaration =>
            Disp_Token (Ctxt, Tok_Sequence);
            Disp_Ident (Ctxt, Get_Identifier (Decl));
            Disp_Psl_Parameter_List (Ctxt, Decl);
            Disp_Token (Ctxt, Tok_Is);
            Print_Sequence (Ctxt, Get_Sequence (Decl));
            Disp_Token (Ctxt, Tok_Semi_Colon);
         when N_Endpoint_Declaration =>
            Disp_Token (Ctxt, Tok_Psl_Endpoint);
            Disp_Ident (Ctxt, Get_Identifier (Decl));
            Disp_Psl_Parameter_List (Ctxt, Decl);
            Disp_Token (Ctxt, Tok_Is);
            Print_Sequence (Ctxt, Get_Sequence (Decl));
            Disp_Token (Ctxt, Tok_Semi_Colon);
         when others =>
            PSL.Errors.Error_Kind ("disp_psl_declaration", Decl);
      end case;
      Close_Hbox (Ctxt);
      case Get_Kind (Decl) is
         when N_Endpoint_Declaration =>
            Disp_PSL_NFA (Get_PSL_NFA (Stmt));
         when others =>
            null;
      end case;
   end Disp_Psl_Declaration;

   procedure Disp_Declaration_Chain
     (Ctxt : in out Ctxt_Class; Parent : Iir)
   is
      Decl: Iir;
   begin
      Decl := Get_Declaration_Chain (Parent);
      while Decl /= Null_Iir loop
         case Get_Kind (Decl) is
            when Iir_Kind_Type_Declaration =>
               Disp_Type_Declaration (Ctxt, Decl);
            when Iir_Kind_Anonymous_Type_Declaration =>
               Disp_Anonymous_Type_Declaration (Ctxt, Decl);
            when Iir_Kind_Subtype_Declaration =>
               Disp_Subtype_Declaration (Ctxt, Decl);
            when Iir_Kind_Use_Clause =>
               Disp_Use_Clause (Ctxt, Decl);
            when Iir_Kind_Component_Declaration =>
               Disp_Component_Declaration (Ctxt, Decl);
            when Iir_Kind_File_Declaration
              | Iir_Kind_Signal_Declaration
              | Iir_Kind_Constant_Declaration
              | Iir_Kind_Variable_Declaration
              | Iir_Kind_Free_Quantity_Declaration
              | Iir_Kinds_Source_Quantity_Declaration =>
               Disp_Object_Declaration (Ctxt, Decl);
               while Get_Has_Identifier_List (Decl) loop
                  Decl := Get_Chain (Decl);
               end loop;
            when Iir_Kind_Object_Alias_Declaration =>
               Disp_Object_Alias_Declaration (Ctxt, Decl);
            when Iir_Kind_Terminal_Declaration =>
               Disp_Terminal_Declaration (Ctxt, Decl);
               while Get_Has_Identifier_List (Decl) loop
                  Decl := Get_Chain (Decl);
               end loop;
            when Iir_Kind_Across_Quantity_Declaration
              | Iir_Kind_Through_Quantity_Declaration =>
               Disp_Branch_Quantity_Declaration (Ctxt, Decl);
               while Get_Has_Identifier_List (Decl) loop
                  Decl := Get_Chain (Decl);
               end loop;
            when Iir_Kind_Nature_Declaration =>
               Disp_Nature_Declaration (Ctxt, Decl);
               declare
                  Def : constant Iir := Get_Nature_Definition (Decl);
               begin
                  if Get_Kind (Def) = Iir_Kind_Scalar_Nature_Definition
                    and then Get_Reference (Def) = Get_Chain (Decl)
                  then
                     Decl := Get_Chain (Decl);
                  end if;
               end;
            when Iir_Kind_Subnature_Declaration =>
               Disp_Subnature_Declaration (Ctxt, Decl);
            when Iir_Kind_Non_Object_Alias_Declaration =>
               Disp_Non_Object_Alias_Declaration (Ctxt, Decl);
            when Iir_Kind_Function_Declaration
              | Iir_Kind_Procedure_Declaration =>
               declare
                  Implicit : constant Boolean :=
                    Is_Implicit_Subprogram (Decl)
                    and then (Get_Implicit_Definition (Decl)
                                /= Iir_Predefined_Now_Function);
               begin
                  if not Implicit or else Flag_Implicit then
                     Start_Hbox (Ctxt);
                     Disp_Subprogram_Declaration (Ctxt, Decl, Implicit);
                     if not Get_Has_Body (Decl) then
                        Disp_Token (Ctxt, Tok_Semi_Colon);
                        Close_Hbox (Ctxt);
                     end if;
                  end if;
               end;
            when Iir_Kind_Function_Body
              | Iir_Kind_Procedure_Body =>
               --  The declaration was just displayed.
               Close_Hbox (Ctxt);
               Start_Hbox (Ctxt);
               Disp_Token (Ctxt, Tok_Is);
               Close_Hbox (Ctxt);
               Disp_Subprogram_Body (Ctxt, Decl);
            when Iir_Kind_Protected_Type_Body =>
               Disp_Protected_Type_Body (Ctxt, Decl);
            when Iir_Kind_Configuration_Specification =>
               Disp_Configuration_Specification (Ctxt, Decl);
            when Iir_Kind_Disconnection_Specification =>
               Disp_Disconnection_Specification (Ctxt, Decl);
            when Iir_Kind_Step_Limit_Specification =>
               Disp_Step_Limit_Specification (Ctxt, Decl);
            when Iir_Kind_Attribute_Declaration =>
               Disp_Attribute_Declaration (Ctxt, Decl);
            when Iir_Kind_Attribute_Specification =>
               Disp_Attribute_Specification (Ctxt, Decl);
            when Iir_Kind_Attribute_Implicit_Declaration =>
               null;
            when Iir_Kind_Group_Template_Declaration =>
               Disp_Group_Template_Declaration (Ctxt, Decl);
            when Iir_Kind_Group_Declaration =>
               Disp_Group_Declaration (Ctxt, Decl);
            when Iir_Kind_Package_Declaration =>
               Disp_Package_Declaration (Ctxt, Decl);
            when Iir_Kind_Package_Body =>
               Disp_Package_Body (Ctxt, Decl);
            when Iir_Kind_Package_Instantiation_Declaration =>
               Disp_Package_Instantiation_Declaration (Ctxt, Decl);
            when Iir_Kind_Psl_Default_Clock =>
               Disp_Psl_Default_Clock (Ctxt, Decl);
            when others =>
               Error_Kind ("disp_declaration_chain", Decl);
         end case;
         Decl := Get_Chain (Decl);
      end loop;
   end Disp_Declaration_Chain;

   procedure Disp_Waveform
     (Ctxt : in out Ctxt_Class; Chain : Iir_Waveform_Element)
   is
      We: Iir_Waveform_Element;
      Val : Iir;
   begin
      if Chain = Null_Iir then
         Disp_Token (Ctxt, Tok_Null);
         --  Put ("null after {disconnection_time}");
         return;
      elsif Get_Kind (Chain) = Iir_Kind_Unaffected_Waveform then
         Disp_Token (Ctxt, Tok_Unaffected);
         return;
      end if;
      We := Chain;
      while We /= Null_Iir loop
         if We /= Chain then
            Disp_Token (Ctxt, Tok_Comma);
         end if;
         Val := Get_We_Value (We);
         Print (Ctxt, Val);
         if Get_Time (We) /= Null_Iir then
            Disp_Token (Ctxt, Tok_After);
            Print (Ctxt, Get_Time (We));
         end if;
         We := Get_Chain (We);
      end loop;
   end Disp_Waveform;

   procedure Disp_Delay_Mechanism (Ctxt : in out Ctxt_Class; Stmt: Iir) is
      Expr: Iir;
   begin
      case Get_Delay_Mechanism (Stmt) is
         when Iir_Transport_Delay =>
            Disp_Token (Ctxt, Tok_Transport);
         when Iir_Inertial_Delay =>
            Expr := Get_Reject_Time_Expression (Stmt);
            if Expr /= Null_Iir then
               Disp_Token (Ctxt, Tok_Reject);
               Print (Ctxt, Expr);
               Disp_Token (Ctxt, Tok_Inertial);
            elsif Get_Has_Delay_Mechanism (Stmt) then
               Disp_Token (Ctxt, Tok_Inertial);
            end if;
      end case;
   end Disp_Delay_Mechanism;

   procedure Disp_Label (Ctxt : in out Ctxt_Class; Stmt : Iir)
   is
      Label: constant Name_Id := Get_Label (Stmt);
   begin
      if Label /= Null_Identifier then
         Disp_Identifier (Ctxt, Stmt);
         Disp_Token (Ctxt, Tok_Colon);
      end if;
   end Disp_Label;

   procedure Disp_Simple_Signal_Assignment
     (Ctxt : in out Ctxt_Class; Stmt: Iir) is
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Stmt);
      Print (Ctxt, Get_Target (Stmt));
      Disp_Token (Ctxt, Tok_Less_Equal);
      Disp_Delay_Mechanism (Ctxt, Stmt);
      Disp_Waveform (Ctxt, Get_Waveform_Chain (Stmt));
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Simple_Signal_Assignment;

   procedure Disp_Conditional_Waveform (Ctxt : in out Ctxt_Class; Chain : Iir)
   is
      Cond_Wf : Iir;
      Expr : Iir;
   begin
      Cond_Wf := Chain;
      loop
         Disp_Waveform (Ctxt, Get_Waveform_Chain (Cond_Wf));
         Expr := Get_Condition (Cond_Wf);
         if Expr /= Null_Iir then
            Disp_Token (Ctxt, Tok_When);
            Print (Ctxt, Expr);
         end if;
         Cond_Wf := Get_Chain (Cond_Wf);
         exit when Cond_Wf = Null_Iir;
         Disp_Token (Ctxt, Tok_Else);
      end loop;
   end Disp_Conditional_Waveform;

   procedure Disp_Conditional_Signal_Assignment
     (Ctxt : in out Ctxt_Class; Stmt: Iir) is
   begin
      Start_Hbox (Ctxt);
      Print (Ctxt, Get_Target (Stmt));
      Disp_Token (Ctxt, Tok_Less_Equal);
      Disp_Delay_Mechanism (Ctxt, Stmt);
      Disp_Conditional_Waveform (Ctxt, Get_Conditional_Waveform_Chain (Stmt));
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Conditional_Signal_Assignment;

   procedure Disp_Selected_Waveforms
     (Ctxt : in out Ctxt_Class; Stmt : Iir)
   is
      Assoc_Chain : constant Iir := Get_Selected_Waveform_Chain (Stmt);
      Assoc: Iir;
   begin
      Assoc := Assoc_Chain;
      while Assoc /= Null_Iir loop
         if Assoc /= Assoc_Chain then
            Disp_Token (Ctxt, Tok_Comma);
         end if;
         Disp_Waveform (Ctxt, Get_Associated_Chain (Assoc));
         Disp_Token (Ctxt, Tok_When);
         Disp_Choice (Ctxt, Assoc);
      end loop;
      Disp_Token (Ctxt, Tok_Semi_Colon);
   end Disp_Selected_Waveforms;

   procedure Disp_Selected_Waveform_Assignment
     (Ctxt : in out Ctxt_Class; Stmt: Iir) is
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Stmt);
      Disp_Token (Ctxt, Tok_With);
      Print (Ctxt, Get_Expression (Stmt));
      Disp_Token (Ctxt, Tok_Select);
      Print (Ctxt, Get_Target (Stmt));
      Disp_Token (Ctxt, Tok_Less_Equal);
      Disp_Delay_Mechanism (Ctxt, Stmt);
      Disp_Selected_Waveforms (Ctxt, Stmt);
      Close_Hbox (Ctxt);
   end Disp_Selected_Waveform_Assignment;

   procedure Disp_Variable_Assignment
     (Ctxt : in out Ctxt_Class; Stmt: Iir) is
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Stmt);
      Print (Ctxt, Get_Target (Stmt));
      Disp_Token (Ctxt, Tok_Assign);
      Print (Ctxt, Get_Expression (Stmt));
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Variable_Assignment;

   procedure Disp_Conditional_Expression_Chain
     (Ctxt : in out Ctxt_Class; Exprs : Iir)
   is
      Expr : Iir;
      Cond : Iir;
   begin
      Expr := Exprs;
      loop
         Print (Ctxt, Get_Expression (Expr));
         Cond := Get_Condition (Expr);
         if Cond /= Null_Iir then
            Disp_Token (Ctxt, Tok_When);
            Print (Ctxt, Cond);
         end if;
         Expr := Get_Chain (Expr);
         exit when Expr = Null_Iir;
         Disp_Token (Ctxt, Tok_Else);
      end loop;
   end Disp_Conditional_Expression_Chain;

   procedure Disp_Conditional_Variable_Assignment
     (Ctxt : in out Ctxt_Class; Stmt: Iir) is
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Stmt);
      Print (Ctxt, Get_Target (Stmt));
      Disp_Token (Ctxt, Tok_Assign);
      Disp_Conditional_Expression_Chain
        (Ctxt, Get_Conditional_Expression_Chain (Stmt));
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Conditional_Variable_Assignment;

   procedure Disp_Postponed (Ctxt : in out Ctxt_Class; Stmt : Iir) is
   begin
      if Get_Postponed_Flag (Stmt) then
         Disp_Token (Ctxt, Tok_Postponed);
      end if;
   end Disp_Postponed;

   procedure Disp_Concurrent_Simple_Signal_Assignment
     (Ctxt : in out Ctxt_Class; Stmt: Iir) is
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Stmt);
      Disp_Postponed (Ctxt, Stmt);
      Print (Ctxt, Get_Target (Stmt));
      Disp_Token (Ctxt, Tok_Less_Equal);
      if Get_Guard (Stmt) /= Null_Iir then
         Disp_Token (Ctxt, Tok_Guarded);
      end if;
      Disp_Delay_Mechanism (Ctxt, Stmt);
      Disp_Waveform (Ctxt, Get_Waveform_Chain (Stmt));

      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Concurrent_Simple_Signal_Assignment;

   procedure Disp_Concurrent_Selected_Signal_Assignment
     (Ctxt : in out Ctxt_Class; Stmt: Iir) is
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Stmt);
      Disp_Postponed (Ctxt, Stmt);
      Disp_Token (Ctxt, Tok_With);
      Print (Ctxt, Get_Expression (Stmt));
      Disp_Token (Ctxt, Tok_Select);
      Print (Ctxt, Get_Target (Stmt));
      Disp_Token (Ctxt, Tok_Less_Equal);
      if Get_Guard (Stmt) /= Null_Iir then
         Disp_Token (Ctxt, Tok_Guarded);
      end if;
      Disp_Delay_Mechanism (Ctxt, Stmt);
      Disp_Selected_Waveforms (Ctxt, Stmt);
      Close_Hbox (Ctxt);
   end Disp_Concurrent_Selected_Signal_Assignment;

   procedure Disp_Concurrent_Conditional_Signal_Assignment
     (Ctxt : in out Ctxt_Class; Stmt: Iir) is
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Stmt);
      Disp_Postponed (Ctxt, Stmt);
      Print (Ctxt, Get_Target (Stmt));
      Disp_Token (Ctxt, Tok_Less_Equal);
      if Get_Guard (Stmt) /= Null_Iir then
         Disp_Token (Ctxt, Tok_Guarded);
      end if;
      Disp_Delay_Mechanism (Ctxt, Stmt);
      Disp_Conditional_Waveform (Ctxt, Get_Conditional_Waveform_Chain (Stmt));
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Concurrent_Conditional_Signal_Assignment;

   procedure Disp_Break_Statement (Ctxt : in out Ctxt_Class; Stmt: Iir)
   is
      List : Iir_List;
      El : Iir;
      Sel : Iir;
      Cond : Iir;
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Stmt);
      Disp_Token (Ctxt, Tok_Break);

      El := Get_Break_Element (Stmt);
      if El /= Null_Iir then
         loop
            Sel := Get_Selector_Quantity (El);
            if Sel /= Null_Iir then
               Disp_Token (Ctxt, Tok_For);
               Print (Ctxt, Sel);
               Disp_Token (Ctxt, Tok_Use);
            end if;
            Print (Ctxt, Get_Break_Quantity (El));
            Disp_Token (Ctxt, Tok_Double_Arrow);
            Print (Ctxt, Get_Expression (El));
            El := Get_Chain (El);
            exit when El = Null_Iir;
            Disp_Token (Ctxt, Tok_Comma);
         end loop;
      end if;

      if Get_Kind (Stmt) = Iir_Kind_Concurrent_Break_Statement then
         List := Get_Sensitivity_List (Stmt);
         if List /= Null_Iir_List then
            Disp_Token (Ctxt, Tok_On);
            Disp_Designator_List (Ctxt, List);
         end if;
      end if;

      Cond := Get_Condition (Stmt);
      if Cond /= Null_Iir then
         Disp_Token (Ctxt, Tok_When);
         Print (Ctxt, Cond);
      end if;
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Break_Statement;

   procedure Disp_Severity_Expression (Ctxt : in out Ctxt_Class; Stmt : Iir)
   is
      Expr : constant Iir := Get_Severity_Expression (Stmt);
   begin
      if Expr /= Null_Iir then
         Disp_Token (Ctxt, Tok_Severity);
         Print (Ctxt, Expr);
      end if;
   end Disp_Severity_Expression;

   procedure Disp_Report_Expression (Ctxt : in out Ctxt_Class; Stmt : Iir)
   is
      Expr : constant Iir := Get_Report_Expression (Stmt);
   begin
      if Expr /= Null_Iir then
         Disp_Token (Ctxt, Tok_Report);
         Print (Ctxt, Expr);
      end if;
   end Disp_Report_Expression;

   procedure Disp_Assertion_Statement (Ctxt : in out Ctxt_Class; Stmt: Iir) is
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Stmt);
      if Get_Kind (Stmt) = Iir_Kind_Concurrent_Assertion_Statement then
         Disp_Postponed (Ctxt, Stmt);
      end if;
      Disp_Token (Ctxt, Tok_Assert);
      Print (Ctxt, Get_Assertion_Condition (Stmt));
      Disp_Report_Expression (Ctxt, Stmt);
      Disp_Severity_Expression (Ctxt, Stmt);
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Assertion_Statement;

   procedure Disp_Report_Statement (Ctxt : in out Ctxt_Class; Stmt: Iir) is
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Stmt);
      Disp_Token (Ctxt, Tok_Report);
      Print (Ctxt, Get_Report_Expression (Stmt));
      Disp_Severity_Expression (Ctxt, Stmt);
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Report_Statement;

   function Get_Operator_Token (Op : Iir) return Token_Type is
   begin
      case Get_Kind (Op) is
         when Iir_Kind_And_Operator
           | Iir_Kind_Reduction_And_Operator =>
            return Tok_And;
         when Iir_Kind_Or_Operator
           | Iir_Kind_Reduction_Or_Operator =>
            return Tok_Or;
         when Iir_Kind_Nand_Operator
           | Iir_Kind_Reduction_Nand_Operator =>
            return Tok_Nand;
         when Iir_Kind_Nor_Operator
           | Iir_Kind_Reduction_Nor_Operator =>
            return Tok_Nor;
         when Iir_Kind_Xor_Operator
           | Iir_Kind_Reduction_Xor_Operator =>
            return Tok_Xor;
         when Iir_Kind_Xnor_Operator
           | Iir_Kind_Reduction_Xnor_Operator =>
            return Tok_Xnor;

         when Iir_Kind_Equality_Operator =>
            return Tok_Equal;
         when Iir_Kind_Inequality_Operator =>
            return Tok_Not_Equal;
         when Iir_Kind_Less_Than_Operator =>
            return Tok_Less;
         when Iir_Kind_Less_Than_Or_Equal_Operator =>
            return Tok_Less_Equal;
         when Iir_Kind_Greater_Than_Operator =>
            return Tok_Greater;
         when Iir_Kind_Greater_Than_Or_Equal_Operator =>
            return Tok_Greater_Equal;

         when Iir_Kind_Match_Equality_Operator =>
            return Tok_Match_Equal;
         when Iir_Kind_Match_Inequality_Operator =>
            return Tok_Match_Not_Equal;
         when Iir_Kind_Match_Less_Than_Operator =>
            return Tok_Match_Less;
         when Iir_Kind_Match_Less_Than_Or_Equal_Operator =>
            return Tok_Match_Less_Equal;
         when Iir_Kind_Match_Greater_Than_Operator =>
            return Tok_Match_Greater;
         when Iir_Kind_Match_Greater_Than_Or_Equal_Operator =>
            return Tok_Match_Greater_Equal;

         when Iir_Kind_Sll_Operator =>
            return Tok_Sll;
         when Iir_Kind_Sla_Operator =>
            return Tok_Sla;
         when Iir_Kind_Srl_Operator =>
            return Tok_Srl;
         when Iir_Kind_Sra_Operator =>
            return Tok_Sra;
         when Iir_Kind_Rol_Operator =>
            return Tok_Rol;
         when Iir_Kind_Ror_Operator =>
            return Tok_Ror;

         when Iir_Kind_Addition_Operator =>
            return Tok_Plus;
         when Iir_Kind_Substraction_Operator =>
            return Tok_Minus;
         when Iir_Kind_Concatenation_Operator =>
            return Tok_Ampersand;
         when Iir_Kind_Multiplication_Operator =>
            return Tok_Star;
         when Iir_Kind_Division_Operator =>
            return Tok_Slash;
         when Iir_Kind_Modulus_Operator =>
            return Tok_Mod;
         when Iir_Kind_Remainder_Operator =>
            return Tok_Rem;
         when Iir_Kind_Exponentiation_Operator =>
            return Tok_Double_Star;
         when Iir_Kind_Not_Operator =>
            return Tok_Not;
         when Iir_Kind_Negation_Operator =>
            return Tok_Minus;
         when Iir_Kind_Identity_Operator =>
            return Tok_Plus;
         when Iir_Kind_Absolute_Operator =>
            return Tok_Abs;
         when Iir_Kind_Condition_Operator
           | Iir_Kind_Implicit_Condition_Operator =>
            return Tok_Condition;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Operator_Token;

   procedure Disp_Dyadic_Operator (Ctxt : in out Ctxt_Class; Expr: Iir) is
   begin
      if Flag_Parenthesis then
         Disp_Token (Ctxt, Tok_Left_Paren);
      end if;
      Print (Ctxt, Get_Left (Expr));
      Disp_Token (Ctxt, Get_Operator_Token (Expr));
      Print (Ctxt, Get_Right (Expr));
      if Flag_Parenthesis then
         Disp_Token (Ctxt, Tok_Right_Paren);
      end if;
   end Disp_Dyadic_Operator;

   procedure Disp_Monadic_Operator (Ctxt : in out Ctxt_Class; Expr: Iir) is
   begin
      if Get_Kind (Expr) = Iir_Kind_Implicit_Condition_Operator then
         Print (Ctxt, Get_Operand (Expr));
         return;
      end if;

      Disp_Token (Ctxt, Get_Operator_Token (Expr));
      if Flag_Parenthesis then
         Disp_Token (Ctxt, Tok_Left_Paren);
      end if;
      Print (Ctxt, Get_Operand (Expr));
      if Flag_Parenthesis then
         Disp_Token (Ctxt, Tok_Right_Paren);
      end if;
   end Disp_Monadic_Operator;

   procedure Disp_Case_Statement
     (Ctxt : in out Ctxt_Class; Stmt: Iir_Case_Statement)
   is
      Assoc: Iir;
      Sel_Stmt : Iir;
   begin
      Disp_Token (Ctxt, Tok_Case);
      Print (Ctxt, Get_Expression (Stmt));
      Close_Hbox (Ctxt);
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Is);
      Close_Hbox (Ctxt);

      Start_Vbox (Ctxt);
      Assoc := Get_Case_Statement_Alternative_Chain (Stmt);
      while Assoc /= Null_Iir loop
         Start_Hbox (Ctxt);
         Disp_Token (Ctxt, Tok_When);
         Sel_Stmt := Get_Associated_Chain (Assoc);
         Disp_Choice (Ctxt, Assoc);
         Disp_Token (Ctxt, Tok_Double_Arrow);
         Close_Hbox (Ctxt);

         Start_Vbox (Ctxt);
         Disp_Sequential_Statements (Ctxt, Sel_Stmt);
         Close_Vbox (Ctxt);
      end loop;
      Close_Vbox (Ctxt);

      Disp_End_Label_No_Close (Ctxt, Stmt, Tok_Case);
   end Disp_Case_Statement;

   procedure Disp_Wait_Statement
     (Ctxt : in out Ctxt_Class; Stmt: Iir_Wait_Statement)
   is
      List: Iir_List;
      Expr: Iir;
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Stmt);
      Disp_Token (Ctxt, Tok_Wait);
      List := Get_Sensitivity_List (Stmt);
      if List /= Null_Iir_List then
         Disp_Token (Ctxt, Tok_On);
         Disp_Designator_List (Ctxt, List);
      end if;
      Expr := Get_Condition_Clause (Stmt);
      if Expr /= Null_Iir then
         Disp_Token (Ctxt, Tok_Until);
         Print (Ctxt, Expr);
      end if;
      Expr := Get_Timeout_Clause (Stmt);
      if Expr /= Null_Iir then
         Disp_Token (Ctxt, Tok_For);
         Print (Ctxt, Expr);
      end if;
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Wait_Statement;

   procedure Disp_If_Statement
     (Ctxt : in out Ctxt_Class; Stmt : Iir_If_Statement)
   is
      Clause : Iir;
      Expr : Iir;
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Stmt);
      Disp_Token (Ctxt, Tok_If);
      Clause := Stmt;
      Print (Ctxt, Get_Condition (Clause));
      Close_Hbox (Ctxt);
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Then);
      Close_Hbox (Ctxt);
      while Clause /= Null_Iir loop
         Start_Vbox (Ctxt);
         Disp_Sequential_Statements
           (Ctxt, Get_Sequential_Statement_Chain (Clause));
         Close_Vbox (Ctxt);
         Clause := Get_Else_Clause (Clause);
         exit when Clause = Null_Iir;
         Start_Hbox (Ctxt);
         Expr := Get_Condition (Clause);
         if Expr /= Null_Iir then
            Disp_Token (Ctxt, Tok_Elsif);
            Print (Ctxt, Expr);
            Close_Hbox (Ctxt);
            Start_Hbox (Ctxt);
            Disp_Token (Ctxt, Tok_Then);
         else
            Disp_Token (Ctxt, Tok_Else);
         end if;
         Close_Hbox (Ctxt);
      end loop;
      Disp_End_Label (Ctxt, Stmt, Tok_If);
   end Disp_If_Statement;

   procedure Disp_Parameter_Specification
     (Ctxt : in out Ctxt_Class; Iterator : Iir_Iterator_Declaration) is
   begin
      Disp_Identifier (Ctxt, Iterator);
      Disp_Token (Ctxt, Tok_In);
      Disp_Discrete_Range (Ctxt, Or_Else (Get_Discrete_Range (Iterator),
                                          Get_Subtype_Indication (Iterator)));
   end Disp_Parameter_Specification;

   procedure Disp_Procedure_Call (Ctxt : in out Ctxt_Class; Stmt : Iir)
   is
      Call : constant Iir := Get_Procedure_Call (Stmt);
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Stmt);
      if Get_Kind (Stmt) = Iir_Kind_Concurrent_Procedure_Call_Statement then
         Disp_Postponed (Ctxt, Stmt);
      end if;
      Print (Ctxt, Get_Prefix (Call));
      Disp_Association_Chain (Ctxt, Get_Parameter_Association_Chain (Call));
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Procedure_Call;

   procedure Disp_For_Loop_Statement (Ctxt : in out Ctxt_Class; Stmt : Iir) is
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Stmt);
      Disp_Token (Ctxt, Tok_For);
      Disp_Parameter_Specification (Ctxt, Get_Parameter_Specification (Stmt));
      Disp_Token (Ctxt, Tok_Loop);
      Close_Hbox (Ctxt);

      Start_Vbox (Ctxt);
      Disp_Sequential_Statements (Ctxt, Get_Sequential_Statement_Chain (Stmt));
      Close_Vbox (Ctxt);

      Disp_End_Label (Ctxt, Stmt, Tok_Loop);
   end Disp_For_Loop_Statement;

   procedure Disp_Force_Mode_Opt (Ctxt : in out Ctxt_Class; Stmt : Iir) is
   begin
      if Get_Has_Force_Mode (Stmt) then
         case Get_Force_Mode (Stmt) is
            when Iir_Force_In =>
               Disp_Token (Ctxt, Tok_In);
            when Iir_Force_Out =>
               Disp_Token (Ctxt, Tok_Out);
         end case;
      end if;
   end Disp_Force_Mode_Opt;

   procedure Disp_Sequential_Statements (Ctxt : in out Ctxt_Class; First : Iir)
   is
      Stmt: Iir;
   begin
      Stmt := First;
      while Stmt /= Null_Iir loop
         case Iir_Kinds_Sequential_Statement (Get_Kind (Stmt)) is
            when Iir_Kind_Null_Statement =>
               Start_Hbox (Ctxt);
               Disp_Label (Ctxt, Stmt);
               Disp_Token (Ctxt, Tok_Null, Tok_Semi_Colon);
               Close_Hbox (Ctxt);
            when Iir_Kind_If_Statement =>
               Disp_If_Statement (Ctxt, Stmt);
            when Iir_Kind_For_Loop_Statement =>
               Disp_For_Loop_Statement (Ctxt, Stmt);
            when Iir_Kind_While_Loop_Statement =>
               declare
                  Cond : constant Iir := Get_Condition (Stmt);
               begin
                  --  As an HBox cannot be empty, check before opening it.
                  if Get_Label (Stmt) /= Null_Identifier
                    or else Cond /= Null_Iir
                  then
                     Start_Hbox (Ctxt);
                     Disp_Label (Ctxt, Stmt);
                     if Get_Condition (Stmt) /= Null_Iir then
                        Disp_Token (Ctxt, Tok_While);
                        Print (Ctxt, Get_Condition (Stmt));
                     end if;
                     Close_Hbox (Ctxt);
                  end if;
               end;
               Start_Hbox (Ctxt);
               Disp_Token (Ctxt, Tok_Loop);
               Close_Hbox (Ctxt);
               Start_Vbox (Ctxt);
               Disp_Sequential_Statements
                 (Ctxt, Get_Sequential_Statement_Chain (Stmt));
               Close_Vbox (Ctxt);
               Disp_End_Label (Ctxt, Stmt, Tok_Loop);
            when Iir_Kind_Simple_Signal_Assignment_Statement =>
               Disp_Simple_Signal_Assignment (Ctxt, Stmt);
            when Iir_Kind_Conditional_Signal_Assignment_Statement =>
               Start_Hbox (Ctxt);
               Disp_Label (Ctxt, Stmt);
               Disp_Conditional_Signal_Assignment (Ctxt, Stmt);
               Close_Hbox (Ctxt);
            when Iir_Kind_Selected_Waveform_Assignment_Statement =>
               Disp_Selected_Waveform_Assignment (Ctxt, Stmt);
            when Iir_Kind_Signal_Force_Assignment_Statement =>
               Start_Hbox (Ctxt);
               Disp_Label (Ctxt, Stmt);
               Print (Ctxt, Get_Target (Stmt));
               Disp_Token (Ctxt, Tok_Less_Equal);
               Disp_Token (Ctxt, Tok_Force);
               Disp_Force_Mode_Opt (Ctxt, Stmt);
               Print (Ctxt, Get_Expression (Stmt));
               Disp_Token (Ctxt, Tok_Semi_Colon);
               Close_Hbox (Ctxt);
            when Iir_Kind_Signal_Release_Assignment_Statement =>
               Start_Hbox (Ctxt);
               Disp_Label (Ctxt, Stmt);
               Print (Ctxt, Get_Target (Stmt));
               Disp_Token (Ctxt, Tok_Less_Equal);
               Disp_Token (Ctxt, Tok_Release);
               Disp_Force_Mode_Opt (Ctxt, Stmt);
               Disp_Token (Ctxt, Tok_Semi_Colon);
               Close_Hbox (Ctxt);
            when Iir_Kind_Variable_Assignment_Statement =>
               Disp_Variable_Assignment (Ctxt, Stmt);
            when Iir_Kind_Conditional_Variable_Assignment_Statement =>
               Disp_Conditional_Variable_Assignment (Ctxt, Stmt);
            when Iir_Kind_Assertion_Statement =>
               Disp_Assertion_Statement (Ctxt, Stmt);
            when Iir_Kind_Report_Statement =>
               Disp_Report_Statement (Ctxt, Stmt);
            when Iir_Kind_Return_Statement =>
               Start_Hbox (Ctxt);
               Disp_Label (Ctxt, Stmt);
               Disp_Token (Ctxt, Tok_Return);
               if Get_Expression (Stmt) /= Null_Iir then
                  Print (Ctxt, Get_Expression (Stmt));
               end if;
               Disp_Token (Ctxt, Tok_Semi_Colon);
               Close_Hbox (Ctxt);
            when Iir_Kind_Case_Statement =>
               Start_Hbox (Ctxt);
               Disp_Label (Ctxt, Stmt);
               Disp_Case_Statement (Ctxt, Stmt);
               Close_Hbox (Ctxt);
            when Iir_Kind_Wait_Statement =>
               Disp_Wait_Statement (Ctxt, Stmt);
            when Iir_Kind_Procedure_Call_Statement =>
               Disp_Procedure_Call (Ctxt, Stmt);
            when Iir_Kind_Exit_Statement
              | Iir_Kind_Next_Statement =>
               declare
                  Label : constant Iir := Get_Loop_Label (Stmt);
                  Cond : constant Iir := Get_Condition (Stmt);
               begin
                  Start_Hbox (Ctxt);
                  Disp_Label (Ctxt, Stmt);
                  if Get_Kind (Stmt) = Iir_Kind_Exit_Statement then
                     Disp_Token (Ctxt, Tok_Exit);
                  else
                     Disp_Token (Ctxt, Tok_Next);
                  end if;
                  if Label /= Null_Iir then
                     Print (Ctxt, Label);
                  end if;
                  if Cond /= Null_Iir then
                     Disp_Token (Ctxt, Tok_When);
                     Print (Ctxt, Cond);
                  end if;
                  Disp_Token (Ctxt, Tok_Semi_Colon);
                  Close_Hbox (Ctxt);
               end;
            when Iir_Kind_Break_Statement =>
               Disp_Break_Statement (Ctxt, Stmt);
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;
   end Disp_Sequential_Statements;

   procedure Disp_Process_Statement (Ctxt : in out Ctxt_Class; Process: Iir) is
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Process);
      Disp_Postponed (Ctxt, Process);

      Disp_Token (Ctxt, Tok_Process);
      if Get_Kind (Process) = Iir_Kind_Sensitized_Process_Statement then
         Disp_Token (Ctxt, Tok_Left_Paren);
         Disp_Designator_List (Ctxt, Get_Sensitivity_List (Process));
         Disp_Token (Ctxt, Tok_Right_Paren);
      end if;
      if Get_Has_Is (Process) then
         Disp_Token (Ctxt, Tok_Is);
      end if;
      Close_Hbox (Ctxt);

      Start_Vbox (Ctxt);
      Disp_Declaration_Chain (Ctxt, Process);
      Close_Vbox (Ctxt);

      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Begin);
      Close_Hbox (Ctxt);

      Start_Vbox (Ctxt);
      Disp_Sequential_Statements
        (Ctxt, Get_Sequential_Statement_Chain (Process));
      Close_Vbox (Ctxt);

      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_End);
      if Get_End_Has_Postponed (Process) then
         Disp_Token (Ctxt, Tok_Postponed);
      end if;
      Disp_After_End (Ctxt, Process, Tok_Process);
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Process_Statement;

   procedure Disp_Conversion (Ctxt : in out Ctxt_Class; Conv : Iir) is
   begin
      case Get_Kind (Conv) is
         when Iir_Kind_Function_Call =>
            Disp_Function_Name (Ctxt, Get_Implementation (Conv));
         when Iir_Kind_Type_Conversion =>
            Disp_Name_Of (Ctxt, Get_Type_Mark (Conv));
         when others =>
            Error_Kind ("disp_conversion", Conv);
      end case;
   end Disp_Conversion;

   procedure Disp_Association_Chain (Ctxt : in out Ctxt_Class; Chain : Iir)
   is
      El: Iir;
      Formal: Iir;
      Need_Comma : Boolean;
      Conv : Iir;
   begin
      if Chain = Null_Iir then
         return;
      end if;
      Disp_Token (Ctxt, Tok_Left_Paren);
      Need_Comma := False;

      El := Chain;
      while El /= Null_Iir loop
         if Get_Kind (El) /= Iir_Kind_Association_Element_By_Individual
           and then not (Get_Kind (El) = Iir_Kind_Association_Element_Open
                           and then Get_Artificial_Flag (El))
         then
            if Need_Comma then
               Disp_Token (Ctxt, Tok_Comma);
            end if;

            --  Formal part.
            if Get_Kind (El) in Iir_Kinds_Association_Element_By_Actual then
               Conv := Get_Formal_Conversion (El);
               if Conv /= Null_Iir then
                  Disp_Conversion (Ctxt, Conv);
                  Disp_Token (Ctxt, Tok_Left_Paren);
               end if;
            else
               Conv := Null_Iir;
            end if;
            Formal := Get_Formal (El);
            if Formal /= Null_Iir then
               Print (Ctxt, Formal);
               if Conv /= Null_Iir then
                  Disp_Token (Ctxt, Tok_Right_Paren);
               end if;
               Disp_Token (Ctxt, Tok_Double_Arrow);
            end if;

            case Iir_Kinds_Association_Element (Get_Kind (El)) is
               when Iir_Kind_Association_Element_Open =>
                  Disp_Token (Ctxt, Tok_Open);
               when Iir_Kind_Association_Element_Package
                 | Iir_Kind_Association_Element_Type
                 | Iir_Kind_Association_Element_Subprogram
                 | Iir_Kind_Association_Element_Terminal =>
                  Print (Ctxt, Get_Actual (El));
               when Iir_Kind_Association_Element_By_Expression
                 | Iir_Kind_Association_Element_By_Name =>
                  Conv := Get_Actual_Conversion (El);
                  if Conv /= Null_Iir then
                     Disp_Conversion (Ctxt, Conv);
                     Disp_Token (Ctxt, Tok_Left_Paren);
                  end if;
                  Print (Ctxt, Get_Actual (El));
                  if Conv /= Null_Iir then
                     Disp_Token (Ctxt, Tok_Right_Paren);
                  end if;
               when Iir_Kind_Association_Element_By_Individual =>
                  raise Program_Error;
            end case;
            Need_Comma := True;
         end if;
         El := Get_Chain (El);
      end loop;
      Disp_Token (Ctxt, Tok_Right_Paren);
   end Disp_Association_Chain;

   procedure Disp_Generic_Map_Aspect
     (Ctxt : in out Ctxt_Class; Parent : Iir) is
   begin
      Disp_Token (Ctxt, Tok_Generic, Tok_Map);
      Disp_Association_Chain (Ctxt, Get_Generic_Map_Aspect_Chain (Parent));
   end Disp_Generic_Map_Aspect;

   procedure Disp_Port_Map_Aspect (Ctxt : in out Ctxt_Class; Parent : Iir) is
   begin
      Disp_Token (Ctxt, Tok_Port, Tok_Map);
      Disp_Association_Chain (Ctxt, Get_Port_Map_Aspect_Chain (Parent));
   end Disp_Port_Map_Aspect;

   procedure Disp_Entity_Aspect (Ctxt : in out Ctxt_Class; Aspect : Iir) is
      Arch : Iir;
   begin
      case Get_Kind (Aspect) is
         when Iir_Kind_Entity_Aspect_Entity =>
            Disp_Token (Ctxt, Tok_Entity);
            Print (Ctxt, Get_Entity_Name (Aspect));
            Arch := Get_Architecture (Aspect);
            if Arch /= Null_Iir then
               Disp_Token (Ctxt, Tok_Left_Paren);
               Disp_Name_Of (Ctxt, Arch);
               Disp_Token (Ctxt, Tok_Right_Paren);
            end if;
         when Iir_Kind_Entity_Aspect_Configuration =>
            Disp_Token (Ctxt, Tok_Configuration);
            Print (Ctxt, Get_Configuration_Name (Aspect));
         when Iir_Kind_Entity_Aspect_Open =>
            Disp_Token (Ctxt, Tok_Open);
         when others =>
            Error_Kind ("disp_entity_aspect", Aspect);
      end case;
   end Disp_Entity_Aspect;

   procedure Disp_Component_Instantiation_Statement
     (Ctxt : in out Ctxt_Class; Stmt: Iir_Component_Instantiation_Statement)
   is
      Component: constant Iir := Get_Instantiated_Unit (Stmt);
      Gen_Map : constant Iir := Get_Generic_Map_Aspect_Chain (Stmt);
      Port_Map : constant Iir := Get_Port_Map_Aspect_Chain (Stmt);
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Stmt);
      if Get_Kind (Component) in Iir_Kinds_Denoting_Name then
         if Get_Has_Component (Stmt) then
            Disp_Token (Ctxt, Tok_Component);
         end if;
         Print (Ctxt, Component);
      else
         Disp_Entity_Aspect (Ctxt, Component);
      end if;

      if Gen_Map = Null_Iir and Port_Map = Null_Iir then
         Disp_Token (Ctxt, Tok_Semi_Colon);
         Close_Hbox (Ctxt);
      else
         Close_Hbox (Ctxt);

         Start_Vbox (Ctxt);
         if Gen_Map /= Null_Iir then
            Start_Hbox (Ctxt);
            Disp_Generic_Map_Aspect (Ctxt, Stmt);
            if Port_Map = Null_Iir then
               Disp_Token (Ctxt, Tok_Semi_Colon);
            end if;
            Close_Hbox (Ctxt);
         end if;

         if Port_Map /= Null_Iir then
            Start_Hbox (Ctxt);
            Disp_Port_Map_Aspect (Ctxt, Stmt);
            Disp_Token (Ctxt, Tok_Semi_Colon);
            Close_Hbox (Ctxt);
         end if;

         Close_Vbox (Ctxt);
      end if;
   end Disp_Component_Instantiation_Statement;

   procedure Disp_Function_Call
     (Ctxt : in out Ctxt_Class; Expr: Iir_Function_Call) is
   begin
      Print (Ctxt, Get_Prefix (Expr));
      Disp_Association_Chain (Ctxt, Get_Parameter_Association_Chain (Expr));
   end Disp_Function_Call;

   procedure Disp_Indexed_Name (Ctxt : in out Ctxt_Class; Indexed: Iir)
   is
      List : Iir_Flist;
      El: Iir;
   begin
      Print (Ctxt, Get_Prefix (Indexed));
      Disp_Token (Ctxt, Tok_Left_Paren);
      List := Get_Index_List (Indexed);
      for I in Flist_First .. Flist_Last (List) loop
         El := Get_Nth_Element (List, I);
         if I /= 0 then
            Disp_Token (Ctxt, Tok_Comma);
         end if;
         Print (Ctxt, El);
      end loop;
      Disp_Token (Ctxt, Tok_Right_Paren);
   end Disp_Indexed_Name;

   procedure Disp_A_Choice (Ctxt : in out Ctxt_Class; Choice : Iir) is
   begin
      case Iir_Kinds_Choice (Get_Kind (Choice)) is
         when Iir_Kind_Choice_By_Others =>
            Disp_Token (Ctxt, Tok_Others);
         when Iir_Kind_Choice_By_None =>
            null;
         when Iir_Kind_Choice_By_Expression =>
            Print (Ctxt, Get_Choice_Expression (Choice));
         when Iir_Kind_Choice_By_Range =>
            Disp_Range (Ctxt, Get_Choice_Range (Choice));
         when Iir_Kind_Choice_By_Name =>
            Disp_Name_Of (Ctxt, Get_Choice_Name (Choice));
      end case;
   end Disp_A_Choice;

   procedure Disp_Choice (Ctxt : in out Ctxt_Class; Choice: in out Iir) is
   begin
      loop
         Disp_A_Choice (Ctxt, Choice);
         Choice := Get_Chain (Choice);
         exit when Choice = Null_Iir;
         exit when Get_Same_Alternative_Flag (Choice) = False;
         --exit when Choice = Null_Iir;
         Disp_Token (Ctxt, Tok_Bar);
      end loop;
   end Disp_Choice;

   --  EL_TYPE is Null_Iir for record aggregates.
   procedure Disp_Aggregate_1
     (Ctxt : in out Ctxt_Class;
      Aggr: Iir_Aggregate; Index : Positive; El_Type : Iir)
   is
      Assoc : Iir;
      Expr : Iir;
      Is_First : Boolean;
   begin
      Disp_Token (Ctxt, Tok_Left_Paren);
      Assoc := Get_Association_Choices_Chain (Aggr);
      Is_First := True;
      while Assoc /= Null_Iir loop
         if Is_First then
            Is_First := False;
         else
            Disp_Token (Ctxt, Tok_Comma);
         end if;
         pragma Assert (not Get_Same_Alternative_Flag (Assoc));
         Expr := Get_Associated_Expr (Assoc);
         Disp_A_Choice (Ctxt, Assoc);
         if Get_Kind (Assoc) /= Iir_Kind_Choice_By_None then
            Assoc := Get_Chain (Assoc);
            while Assoc /= Null_Iir
              and then Get_Same_Alternative_Flag (Assoc)
            loop
               Disp_Token (Ctxt, Tok_Bar);
               Disp_A_Choice (Ctxt, Assoc);
               Assoc := Get_Chain (Assoc);
            end loop;
            Disp_Token (Ctxt, Tok_Double_Arrow);
         else
            Assoc := Get_Chain (Assoc);
         end if;
         if Index > 1 then
            if Get_Kind (Expr) = Iir_Kind_String_Literal8 then
               Disp_String_Literal (Ctxt, Expr, El_Type);
            else
               Disp_Aggregate_1 (Ctxt, Expr, Index - 1, El_Type);
            end if;
         else
            Print (Ctxt, Expr);
         end if;
      end loop;
      Disp_Token (Ctxt, Tok_Right_Paren);
   end Disp_Aggregate_1;

   procedure Disp_Aggregate (Ctxt : in out Ctxt_Class; Aggr: Iir_Aggregate)
   is
      Aggr_Type : constant Iir := Get_Type (Aggr);
      Base_Type : Iir;
   begin
      if Aggr_Type /= Null_Iir
        and then Get_Kind (Aggr_Type) in Iir_Kinds_Array_Type_Definition
      then
         Base_Type := Get_Base_Type (Aggr_Type);
         Disp_Aggregate_1
           (Ctxt, Aggr, Get_Nbr_Elements (Get_Index_Subtype_List (Base_Type)),
            Get_Element_Subtype (Base_Type));
      else
         Disp_Aggregate_1 (Ctxt, Aggr, 1, Null_Iir);
      end if;
   end Disp_Aggregate;

   procedure Disp_Simple_Aggregate
     (Ctxt : in out Ctxt_Class; Aggr: Iir_Simple_Aggregate)
   is
      List : constant Iir_Flist := Get_Simple_Aggregate_List (Aggr);
      El : Iir;
      First : Boolean := True;
   begin
      Disp_Token (Ctxt, Tok_Left_Paren);
      for I in Flist_First .. Flist_Last (List) loop
         El := Get_Nth_Element (List, I);
         if First then
            First := False;
         else
            Disp_Token (Ctxt, Tok_Comma);
         end if;
         Print (Ctxt, El);
      end loop;
      Disp_Token (Ctxt, Tok_Right_Paren);
   end Disp_Simple_Aggregate;

   procedure Disp_Parametered_Attribute
     (Ctxt : in out Ctxt_Class; Name : Name_Id; Expr : Iir)
   is
      Param : Iir;
      Pfx : Iir;
   begin
      Pfx := Get_Prefix (Expr);
      case Get_Kind (Pfx) is
         when Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration =>
            Disp_Name_Of (Ctxt, Pfx);
         when others =>
            Print (Ctxt, Pfx);
      end case;
      Disp_Token (Ctxt, Tok_Tick);
      Disp_Ident (Ctxt, Name);
      Param := Get_Parameter (Expr);
      if Param /= Null_Iir
        and then Param /= Vhdl.Std_Package.Universal_Integer_One
      then
         Disp_Token (Ctxt, Tok_Left_Paren);
         Print (Ctxt, Param);
         Disp_Token (Ctxt, Tok_Right_Paren);
      end if;
   end Disp_Parametered_Attribute;

   procedure Disp_Parametered_Attribute
     (Ctxt : in out Ctxt_Class; Name : Name_Id; Expr : Iir; Num : Natural)
   is
      Param : Iir;
      Pfx : Iir;
      Has_Params : Boolean;
   begin
      Pfx := Get_Prefix (Expr);
      Print (Ctxt, Pfx);
      Disp_Token (Ctxt, Tok_Tick);
      Disp_Ident (Ctxt, Name);
      Has_Params := False;
      for I in 1 .. Num loop
         Param := Get_Attribute_Parameter (Expr, I);
         exit when Param = Null_Iir;
         if not Has_Params then
            Disp_Token (Ctxt, Tok_Left_Paren);
            Has_Params := True;
         else
            Disp_Token (Ctxt, Tok_Comma);
         end if;
         Print (Ctxt, Param);
      end loop;
      if Has_Params then
         Disp_Token (Ctxt, Tok_Right_Paren);
      end if;
   end Disp_Parametered_Attribute;

   procedure Disp_Parametered_Type_Attribute
     (Ctxt : in out Ctxt_Class; Name : Name_Id; Expr : Iir) is
   begin
      Print (Ctxt, Get_Prefix (Expr));
      Disp_Token (Ctxt, Tok_Tick);
      Disp_Ident (Ctxt, Name);
      Disp_Token (Ctxt, Tok_Left_Paren);
      Print (Ctxt, Get_Parameter (Expr));
      Disp_Token (Ctxt, Tok_Right_Paren);
   end Disp_Parametered_Type_Attribute;

   procedure Disp_String_Literal_Raw
     (Ctxt : in out Ctxt_Class; Str : Iir; El_Type : Iir)
   is
      Str_Id : constant String8_Id := Get_String8_Id (Str);
      Len : constant Nat32 := Get_String_Length (Str);
      Literal_List : Iir_Flist;
      Pos : Nat8;
      Lit : Iir;
      Id : Name_Id;
      C : Character;
   begin
      if Get_Bit_String_Base (Str) /= Base_None then
         Start_Lit (Ctxt, Tok_Bit_String);
         if Get_Has_Length (Str) then
            Disp_Int32 (Ctxt, Iir_Int32 (Get_String_Length (Str)));
         end if;
         Disp_Char (Ctxt, 'b');
      else
         Start_Lit (Ctxt, Tok_String);
      end if;

      Disp_Char (Ctxt, '"');

      if El_Type /= Null_Iir then
         Literal_List :=
           Get_Enumeration_Literal_List (Get_Base_Type (El_Type));
      else
         Literal_List := Null_Iir_Flist;
      end if;

      for I in 1 .. Len loop
         Pos := Str_Table.Element_String8 (Str_Id, I);
         if Literal_List /= Null_Iir_Flist then
            Lit := Get_Nth_Element (Literal_List, Natural (Pos));
            Id := Get_Identifier (Lit);
         else
            Id := Name_Table.Get_Identifier (Character'Val (Pos));
         end if;
         pragma Assert (Name_Table.Is_Character (Id));
         C := Name_Table.Get_Character (Id);
         if C = '"' then
            Disp_Char (Ctxt, C);
         end if;
         Disp_Char (Ctxt, C);
      end loop;

      Disp_Char (Ctxt, '"');
      Close_Lit (Ctxt);
   end Disp_String_Literal_Raw;

   procedure Disp_String_Literal
     (Ctxt : in out Ctxt_Class; Str : Iir; El_Type : Iir) is
   begin
      if Get_Literal_Length (Str) /= 0 then
         declare
            Tkind : Token_Type;
         begin
            if Get_Bit_String_Base (Str) /= Base_None then
               Tkind := Tok_Bit_String;
            else
               Tkind := Tok_String;
            end if;
            Disp_Literal_From_Source (Ctxt, Str, Tkind);
         end;
      else
         Disp_String_Literal_Raw (Ctxt, Str, El_Type);
      end if;
   end Disp_String_Literal;

   procedure Disp_Block_Header
     (Ctxt : in out Ctxt_Class; Header : Iir_Block_Header)
   is
      Chain : Iir;
   begin
      if Header = Null_Iir then
         return;
      end if;
      Chain := Get_Generic_Chain (Header);
      if Chain /= Null_Iir then
         Disp_Generics (Ctxt, Header);

         Chain := Get_Generic_Map_Aspect_Chain (Header);
         if Chain /= Null_Iir then
            Start_Hbox (Ctxt);
            Disp_Generic_Map_Aspect (Ctxt, Header);
            Disp_Token (Ctxt, Tok_Semi_Colon);
            Close_Hbox (Ctxt);
         end if;
      end if;
      Chain := Get_Port_Chain (Header);
      if Chain /= Null_Iir then
         Disp_Ports (Ctxt, Header);

         Chain := Get_Port_Map_Aspect_Chain (Header);
         if Chain /= Null_Iir then
            Start_Hbox (Ctxt);
            Disp_Port_Map_Aspect (Ctxt, Header);
            Disp_Token (Ctxt, Tok_Semi_Colon);
            Close_Hbox (Ctxt);
         end if;
      end if;
   end Disp_Block_Header;

   procedure Disp_Block_Statement
     (Ctxt : in out Ctxt_Class; Block: Iir_Block_Statement)
   is
      Sensitivity: Iir_List;
      Guard : Iir_Guard_Signal_Declaration;
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Block);
      Disp_Token (Ctxt, Tok_Block);
      Guard := Get_Guard_Decl (Block);
      if Guard /= Null_Iir then
         Disp_Token (Ctxt, Tok_Left_Paren);
         Print (Ctxt, Get_Guard_Expression (Guard));
         Disp_Token (Ctxt, Tok_Right_Paren);
      end if;
      if Get_Has_Is (Block) then
         Disp_Token (Ctxt, Tok_Is);
      end if;
      Close_Hbox (Ctxt);

      if Flags.List_Verbose and then Guard /= Null_Iir then
         Sensitivity := Get_Guard_Sensitivity_List (Guard);
         if Sensitivity /= Null_Iir_List then
            OOB.Put ("-- guard sensitivity list ");
            Disp_Designator_List (Ctxt, Sensitivity);
         end if;
      end if;

      Start_Vbox (Ctxt);
      Disp_Block_Header (Ctxt, Get_Block_Header (Block));
      Disp_Declaration_Chain (Ctxt, Block);
      Close_Vbox (Ctxt);

      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Begin);
      Close_Hbox (Ctxt);

      Start_Vbox (Ctxt);
      Disp_Concurrent_Statement_Chain (Ctxt, Block);
      Close_Vbox (Ctxt);

      Disp_End (Ctxt, Block, Tok_Block);
   end Disp_Block_Statement;

   procedure Disp_Generate_Statement_Body (Ctxt : in out Ctxt_Class; Bod : Iir)
   is
      Has_Beg : constant Boolean := Get_Has_Begin (Bod);
      Has_End : constant Boolean := Get_Has_End (Bod);
   begin
      Disp_Declaration_Chain (Ctxt, Bod);
      if Has_Beg then
         Start_Hbox (Ctxt);
         Disp_Token (Ctxt, Tok_Begin);
         Close_Hbox (Ctxt);
      end if;

      if Has_Beg or Has_End then
         Start_Vbox (Ctxt);
      end if;
      Disp_Concurrent_Statement_Chain (Ctxt, Bod);
      if Has_Beg or Has_End then
         Close_Vbox (Ctxt);
      end if;

      if Has_End then
         Start_Hbox (Ctxt);
         Disp_Token (Ctxt, Tok_End);
         if Get_End_Has_Identifier (Bod) then
            Disp_Ident (Ctxt, Get_Alternative_Label (Bod));
         end if;
         Disp_Token (Ctxt, Tok_Semi_Colon);
         Close_Hbox (Ctxt);
      end if;
   end Disp_Generate_Statement_Body;

   procedure Disp_For_Generate_Statement
     (Ctxt : in out Ctxt_Class; Stmt : Iir) is
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Stmt);
      Disp_Token (Ctxt, Tok_For);
      Disp_Parameter_Specification (Ctxt, Get_Parameter_Specification (Stmt));
      Disp_Token (Ctxt, Tok_Generate);
      Close_Hbox (Ctxt);

      Start_Vbox (Ctxt);
      Disp_Generate_Statement_Body
        (Ctxt, Get_Generate_Statement_Body (Stmt));
      Close_Vbox (Ctxt);

      Disp_End (Ctxt, Stmt, Tok_Generate);
   end Disp_For_Generate_Statement;

   procedure Disp_If_Generate_Statement (Ctxt : in out Ctxt_Class; Stmt : Iir)
   is
      Bod : Iir;
      Clause : Iir;
      Cond : Iir;
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Stmt);
      Disp_Token (Ctxt, Tok_If);
      Cond := Get_Condition (Stmt);
      Clause := Stmt;
      loop
         Bod := Get_Generate_Statement_Body (Clause);
         if Get_Has_Label (Bod) then
            Disp_Ident (Ctxt, Get_Alternative_Label (Bod));
            Disp_Token (Ctxt, Tok_Colon);
         end if;
         if Cond /= Null_Iir then
            Print (Ctxt, Cond);
         end if;
         Disp_Token (Ctxt, Tok_Generate);
         Close_Hbox (Ctxt);

         Start_Vbox (Ctxt);
         Disp_Generate_Statement_Body (Ctxt, Bod);
         Close_Vbox (Ctxt);

         Clause := Get_Generate_Else_Clause (Clause);
         exit when Clause = Null_Iir;

         Start_Hbox (Ctxt);
         Cond := Get_Condition (Clause);
         if Cond = Null_Iir then
            Disp_Token (Ctxt, Tok_Else);
         else
            Disp_Token (Ctxt, Tok_Elsif);
         end if;
      end loop;
      Disp_End (Ctxt, Stmt, Tok_Generate);
   end Disp_If_Generate_Statement;

   procedure Disp_Case_Generate_Statement
     (Ctxt : in out Ctxt_Class; Stmt : Iir)
   is
      Bod : Iir;
      Assoc : Iir;
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Stmt);
      Disp_Token (Ctxt, Tok_Case);
      Print (Ctxt, Get_Expression (Stmt));
      Disp_Token (Ctxt, Tok_Generate);
      Close_Hbox (Ctxt);

      Start_Vbox (Ctxt);
      Assoc := Get_Case_Statement_Alternative_Chain (Stmt);
      while Assoc /= Null_Iir loop
         Start_Hbox (Ctxt);
         Disp_Token (Ctxt, Tok_When);
         Bod := Get_Associated_Block (Assoc);
         if Get_Has_Label (Bod) then
            Disp_Ident (Ctxt, Get_Alternative_Label (Bod));
            Disp_Token (Ctxt, Tok_Colon);
         end if;
         Disp_Choice (Ctxt, Assoc);
         Disp_Token (Ctxt, Tok_Double_Arrow);
         Close_Hbox (Ctxt);

         Start_Vbox (Ctxt);
         Disp_Generate_Statement_Body (Ctxt, Bod);
         Close_Vbox (Ctxt);
      end loop;
      Close_Vbox (Ctxt);
      Disp_End (Ctxt, Stmt, Tok_Generate);
   end Disp_Case_Generate_Statement;

   procedure Disp_PSL_NFA (Ctxt : in out Ctxt_Class; N : PSL.Nodes.NFA)
   is
      use PSL.NFAs;

      procedure Disp_State (S : NFA_State)
      is
         Str : constant String := Int32'Image (Get_State_Label (S));
         S1 : constant String := NFA_State'Image (S);
      begin
         OOB.Put (Str (2 .. Str'Last));
         OOB.Put ("[");
         OOB.Put (S1 (2 .. S1'Last));
         OOB.Put ("]");
      end Disp_State;

      S : NFA_State;
      E : NFA_Edge;
   begin
      if N /= No_NFA then
         OOB.Put ("-- start: ");
         Disp_State (Get_Start_State (N));
         OOB.Put (", final: ");
         Disp_State (Get_Final_State (N));
         OOB.Put (", active: ");
         S := Get_Active_State (N);
         if S = No_State then
            OOB.Put ("-");
         else
            Disp_State (S);
         end if;
         if Get_Epsilon_NFA (N) then
            OOB.Put (", epsilon");
         end if;
         OOB.New_Line;

         S := Get_First_State (N);
         while S /= No_State loop
            E := Get_First_Src_Edge (S);
            while E /= No_Edge loop
               OOB.Put ("-- ");
               Disp_State (S);
               OOB.Put (" -> ");
               Disp_State (Get_Edge_Dest (E));
               Disp_Token (Ctxt, Tok_Colon);  --  To display ": "
               Disp_Psl_Expression (Ctxt, Get_Edge_Expr (E));
               OOB.New_Line;
               E := Get_Next_Src_Edge (E);
            end loop;
            S := Get_Next_State (S);
         end loop;
      end if;
   end Disp_PSL_NFA;

   procedure Disp_Psl_Assert_Directive
     (Ctxt : in out Ctxt_Class; Stmt : Iir) is
   begin
      Start_Hbox (Ctxt);
      if Vhdl_Std < Vhdl_08 then
         OOB.Put ("--psl ");
      end if;
      Disp_Label (Ctxt, Stmt);
      Disp_Postponed (Ctxt, Stmt);
      Disp_Token (Ctxt, Tok_Assert);
      Disp_Psl_Expression (Ctxt, Get_Psl_Property (Stmt));
      Disp_Report_Expression (Ctxt, Stmt);
      Disp_Severity_Expression (Ctxt, Stmt);
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
      Disp_PSL_NFA (Get_PSL_NFA (Stmt));
   end Disp_Psl_Assert_Directive;

   procedure Disp_Psl_Assume_Directive
     (Ctxt : in out Ctxt_Class; Stmt : Iir) is
   begin
      Start_Hbox (Ctxt);
      if Vhdl_Std < Vhdl_08 then
         OOB.Put ("--psl ");
      end if;
      Disp_Label (Ctxt, Stmt);
      Disp_Postponed (Ctxt, Stmt);
      Disp_Token (Ctxt, Tok_Assume);
      Disp_Psl_Expression (Ctxt, Get_Psl_Property (Stmt));
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
      Disp_PSL_NFA (Get_PSL_NFA (Stmt));
   end Disp_Psl_Assume_Directive;

   procedure Disp_Psl_Cover_Directive
     (Ctxt : in out Ctxt_Class; Stmt : Iir) is
   begin
      Start_Hbox (Ctxt);
      if Vhdl_Std < Vhdl_08 then
         OOB.Put ("--psl ");
      end if;
      Disp_Label (Ctxt, Stmt);
      Disp_Token (Ctxt, Tok_Cover);
      Print_Sequence (Ctxt, Get_Psl_Sequence (Stmt));
      Disp_Report_Expression (Ctxt, Stmt);
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
      Disp_PSL_NFA (Get_PSL_NFA (Stmt));
   end Disp_Psl_Cover_Directive;

   procedure Disp_Psl_Restrict_Directive
     (Ctxt : in out Ctxt_Class; Stmt : Iir) is
   begin
      Start_Hbox (Ctxt);
      if Vhdl_Std < Vhdl_08 then
         OOB.Put ("--psl ");
      end if;
      Disp_Label (Ctxt, Stmt);
      Disp_Token (Ctxt, Tok_Restrict);
      Print_Sequence (Ctxt, Get_Psl_Sequence (Stmt));
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
      Disp_PSL_NFA (Get_PSL_NFA (Stmt));
   end Disp_Psl_Restrict_Directive;

   procedure Disp_Simple_Simultaneous_Statement
     (Ctxt : in out Ctxt_Class; Stmt : Iir) is
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Stmt);
      Print (Ctxt, Get_Simultaneous_Left (Stmt));
      Disp_Token (Ctxt, Tok_Equal_Equal);
      Print (Ctxt, Get_Simultaneous_Right (Stmt));
      Disp_Tolerance_Opt (Ctxt, Stmt);
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Simple_Simultaneous_Statement;

   procedure Disp_Simultaneous_If_Statement
     (Ctxt : in out Ctxt_Class; Stmt : Iir)
   is
      Clause : Iir;
      Expr : Iir;
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Stmt);
      Disp_Token (Ctxt, Tok_If);
      Clause := Stmt;
      Print (Ctxt, Get_Condition (Clause));
      Close_Hbox (Ctxt);
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Use);
      Close_Hbox (Ctxt);
      while Clause /= Null_Iir loop
         Start_Vbox (Ctxt);
         Disp_Simultaneous_Statement_Chain
           (Ctxt, Get_Simultaneous_Statement_Chain (Clause));
         Close_Vbox (Ctxt);
         Clause := Get_Else_Clause (Clause);
         exit when Clause = Null_Iir;
         Start_Hbox (Ctxt);
         Expr := Get_Condition (Clause);
         if Expr /= Null_Iir then
            Disp_Token (Ctxt, Tok_Elsif);
            Print (Ctxt, Expr);
            Close_Hbox (Ctxt);
            Start_Hbox (Ctxt);
            Disp_Token (Ctxt, Tok_Use);
         else
            Disp_Token (Ctxt, Tok_Else);
         end if;
         Close_Hbox (Ctxt);
      end loop;
      Disp_End_Label (Ctxt, Stmt, Tok_Use);
   end Disp_Simultaneous_If_Statement;

   procedure Disp_Simultaneous_Case_Statement
     (Ctxt : in out Ctxt_Class; Stmt : Iir)
   is
      Assoc: Iir;
      Stmts : Iir;
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Stmt);
      Disp_Token (Ctxt, Tok_Case);
      Print (Ctxt, Get_Expression (Stmt));
      Close_Hbox (Ctxt);
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Use);
      Close_Hbox (Ctxt);

      Start_Vbox (Ctxt);
      Assoc := Get_Case_Statement_Alternative_Chain (Stmt);
      while Assoc /= Null_Iir loop
         Start_Hbox (Ctxt);
         Disp_Token (Ctxt, Tok_When);
         Stmts := Get_Associated_Chain (Assoc);
         Disp_Choice (Ctxt, Assoc);
         Disp_Token (Ctxt, Tok_Double_Arrow);
         Close_Hbox (Ctxt);

         Start_Vbox (Ctxt);
         Disp_Simultaneous_Statement_Chain (Ctxt, Stmts);
         Close_Vbox (Ctxt);
      end loop;
      Close_Vbox (Ctxt);

      Disp_End_Label (Ctxt, Stmt, Tok_Case);
   end Disp_Simultaneous_Case_Statement;

   procedure Disp_Simultaneous_Procedural_Statement
     (Ctxt : in out Ctxt_Class; Stmt : Iir) is
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Stmt);

      Disp_Token (Ctxt, Tok_Procedural);
      if Get_Has_Is (Stmt) then
         Disp_Token (Ctxt, Tok_Is);
      end if;
      Close_Hbox (Ctxt);

      Start_Vbox (Ctxt);
      Disp_Declaration_Chain (Ctxt, Stmt);
      Close_Vbox (Ctxt);

      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Begin);
      Close_Hbox (Ctxt);

      Start_Vbox (Ctxt);
      Disp_Sequential_Statements
        (Ctxt, Get_Sequential_Statement_Chain (Stmt));
      Close_Vbox (Ctxt);

      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_End);
      Disp_After_End (Ctxt, Stmt, Tok_Procedural);
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Simultaneous_Procedural_Statement;

   procedure Disp_Simultaneous_Null_Statement
     (Ctxt : in out Ctxt_Class; Stmt : Iir) is
   begin
      Start_Hbox (Ctxt);
      Disp_Label (Ctxt, Stmt);
      Disp_Token (Ctxt, Tok_Null);
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Simultaneous_Null_Statement;

   procedure Disp_Concurrent_Statement (Ctxt : in out Ctxt_Class; Stmt: Iir) is
   begin
      case Get_Kind (Stmt) is
         when Iir_Kind_Concurrent_Simple_Signal_Assignment =>
            Disp_Concurrent_Simple_Signal_Assignment (Ctxt, Stmt);
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
            Disp_Concurrent_Conditional_Signal_Assignment (Ctxt, Stmt);
         when Iir_Kind_Concurrent_Selected_Signal_Assignment =>
            Disp_Concurrent_Selected_Signal_Assignment (Ctxt, Stmt);
         when Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement =>
            Disp_Process_Statement (Ctxt, Stmt);
         when Iir_Kind_Concurrent_Assertion_Statement =>
            Disp_Assertion_Statement (Ctxt, Stmt);
         when Iir_Kind_Component_Instantiation_Statement =>
            Disp_Component_Instantiation_Statement (Ctxt, Stmt);
         when Iir_Kind_Concurrent_Procedure_Call_Statement =>
            Disp_Procedure_Call (Ctxt, Stmt);
         when Iir_Kind_Concurrent_Break_Statement =>
            Disp_Break_Statement (Ctxt, Stmt);
         when Iir_Kind_Block_Statement =>
            Disp_Block_Statement (Ctxt, Stmt);
         when Iir_Kind_If_Generate_Statement =>
            Disp_If_Generate_Statement (Ctxt, Stmt);
         when Iir_Kind_Case_Generate_Statement =>
            Disp_Case_Generate_Statement (Ctxt, Stmt);
         when Iir_Kind_For_Generate_Statement =>
            Disp_For_Generate_Statement (Ctxt, Stmt);
         when Iir_Kind_Psl_Default_Clock =>
            Disp_Psl_Default_Clock (Ctxt, Stmt);
         when Iir_Kind_Psl_Declaration
           | Iir_Kind_Psl_Endpoint_Declaration =>
            Disp_Psl_Declaration (Ctxt, Stmt);
         when Iir_Kind_Psl_Assert_Directive =>
            Disp_Psl_Assert_Directive (Ctxt, Stmt);
         when Iir_Kind_Psl_Assume_Directive =>
            Disp_Psl_Assume_Directive (Ctxt, Stmt);
         when Iir_Kind_Psl_Cover_Directive =>
            Disp_Psl_Cover_Directive (Ctxt, Stmt);
         when Iir_Kind_Psl_Restrict_Directive =>
            Disp_Psl_Restrict_Directive (Ctxt, Stmt);
         when Iir_Kind_Simple_Simultaneous_Statement =>
            Disp_Simple_Simultaneous_Statement (Ctxt, Stmt);
         when Iir_Kind_Simultaneous_If_Statement =>
            Disp_Simultaneous_If_Statement (Ctxt, Stmt);
         when Iir_Kind_Simultaneous_Case_Statement =>
            Disp_Simultaneous_Case_Statement (Ctxt, Stmt);
         when Iir_Kind_Simultaneous_Procedural_Statement =>
            Disp_Simultaneous_Procedural_Statement (Ctxt, Stmt);
         when Iir_Kind_Simultaneous_Null_Statement =>
            Disp_Simultaneous_Null_Statement (Ctxt, Stmt);
         when others =>
            Error_Kind ("disp_concurrent_statement", Stmt);
      end case;
   end Disp_Concurrent_Statement;

   procedure Disp_Package_Declaration
     (Ctxt : in out Ctxt_Class; Decl: Iir_Package_Declaration)
   is
      Header : constant Iir := Get_Package_Header (Decl);
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Package);
      Disp_Identifier (Ctxt, Decl);
      Disp_Token (Ctxt, Tok_Is);
      Close_Hbox (Ctxt);
      Start_Vbox (Ctxt);
      if Header /= Null_Iir then
         Disp_Generics (Ctxt, Header);
      end if;
      Disp_Declaration_Chain (Ctxt, Decl);
      Close_Vbox (Ctxt);
      Disp_End (Ctxt, Decl, Tok_Package);
   end Disp_Package_Declaration;

   procedure Disp_Package_Body (Ctxt : in out Ctxt_Class; Decl: Iir) is
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Package, Tok_Body);
      Disp_Identifier (Ctxt, Decl);
      Disp_Token (Ctxt, Tok_Is);
      Close_Hbox (Ctxt);
      Start_Vbox (Ctxt);
      Disp_Declaration_Chain (Ctxt, Decl);
      Close_Vbox (Ctxt);
      Disp_End (Ctxt, Decl, Tok_Package, Tok_Body);
   end Disp_Package_Body;

   procedure Disp_Package_Instantiation_Declaration
     (Ctxt : in out Ctxt_Class; Decl: Iir) is
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Package);
      Disp_Identifier (Ctxt, Decl);
      Disp_Token (Ctxt, Tok_Is, Tok_New);
      Print (Ctxt, Get_Uninstantiated_Package_Name (Decl));
      Disp_Generic_Map_Aspect (Ctxt, Decl);
      Disp_Token (Ctxt, Tok_Semi_Colon);
      Close_Hbox (Ctxt);
   end Disp_Package_Instantiation_Declaration;

   procedure Disp_Binding_Indication (Ctxt : in out Ctxt_Class; Bind : Iir)
   is
      El : Iir;
   begin
      El := Get_Entity_Aspect (Bind);
      if El /= Null_Iir then
         Disp_Token (Ctxt, Tok_Use);
         Disp_Entity_Aspect (Ctxt, El);
      end if;
      El := Get_Generic_Map_Aspect_Chain (Bind);
      if El /= Null_Iir then
         Disp_Generic_Map_Aspect (Ctxt, Bind);
      end if;
      El := Get_Port_Map_Aspect_Chain (Bind);
      if El /= Null_Iir then
         Disp_Port_Map_Aspect (Ctxt, Bind);
      end if;
   end Disp_Binding_Indication;

   procedure Disp_Component_Configuration
     (Ctxt : in out Ctxt_Class; Conf : Iir_Component_Configuration)
   is
      Block : Iir_Block_Configuration;
      Binding : Iir;
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_For);
      Disp_Instantiation_List (Ctxt, Get_Instantiation_List (Conf));
      Disp_Token (Ctxt, Tok_Colon);
      Print (Ctxt, Get_Component_Name (Conf));
      Close_Hbox (Ctxt);

      Start_Vbox (Ctxt);
      Binding := Get_Binding_Indication (Conf);
      if Binding /= Null_Iir then
         Start_Hbox (Ctxt);
         Disp_Binding_Indication (Ctxt, Binding);
         Disp_Token (Ctxt, Tok_Semi_Colon);
         Close_Hbox (Ctxt);
      end if;
      Block := Get_Block_Configuration (Conf);
      if Block /= Null_Iir then
         Disp_Block_Configuration (Ctxt, Block);
      end if;
      Close_Vbox (Ctxt);

      Disp_End (Ctxt, Tok_For);
   end Disp_Component_Configuration;

   procedure Disp_Configuration_Items
     (Ctxt : in out Ctxt_Class; Conf : Iir_Block_Configuration)
   is
      El : Iir;
   begin
      El := Get_Configuration_Item_Chain (Conf);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Block_Configuration =>
               Disp_Block_Configuration (Ctxt, El);
            when Iir_Kind_Component_Configuration =>
               Disp_Component_Configuration (Ctxt, El);
            when Iir_Kind_Configuration_Specification =>
               --  This may be created by canon.
               Disp_Configuration_Specification (Ctxt, El);
               Disp_Token (Ctxt, Tok_End, Tok_For);
               Disp_Token (Ctxt, Tok_Semi_Colon);
            when others =>
               Error_Kind ("disp_configuration_item_list", El);
         end case;
         El := Get_Chain (El);
      end loop;
   end Disp_Configuration_Items;

   procedure Disp_Block_Configuration
     (Ctxt : in out Ctxt_Class; Block: Iir_Block_Configuration)
   is
      Spec : Iir;
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_For);
      Spec := Get_Block_Specification (Block);
      case Get_Kind (Spec) is
         when Iir_Kind_Block_Statement
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_Architecture_Body =>
            Disp_Name_Of (Ctxt, Spec);
         when Iir_Kind_Indexed_Name =>
            declare
               Index_List : constant Iir_Flist := Get_Index_List (Spec);
            begin
               Disp_Name_Of (Ctxt, Get_Prefix (Spec));
               Disp_Token (Ctxt, Tok_Left_Paren);
               if Index_List = Iir_Flist_Others then
                  Disp_Token (Ctxt, Tok_Others);
               else
                  Print (Ctxt, Get_Nth_Element (Index_List, 0));
               end if;
               Disp_Token (Ctxt, Tok_Right_Paren);
            end;
         when Iir_Kind_Slice_Name =>
            Disp_Name_Of (Ctxt, Get_Prefix (Spec));
            Disp_Token (Ctxt, Tok_Left_Paren);
            Disp_Range (Ctxt, Get_Suffix (Spec));
            Disp_Token (Ctxt, Tok_Right_Paren);
         when Iir_Kind_Simple_Name
           | Iir_Kind_Parenthesis_Name =>
            Print (Ctxt, Spec);
         when others =>
            Error_Kind ("disp_block_configuration", Spec);
      end case;
      Close_Hbox (Ctxt);

      Start_Vbox (Ctxt);
      Disp_Declaration_Chain (Ctxt, Block);
      Disp_Configuration_Items (Ctxt, Block);
      Close_Vbox (Ctxt);
      Disp_End (Ctxt, Tok_For);
   end Disp_Block_Configuration;

   procedure Disp_Configuration_Declaration
     (Ctxt : in out Ctxt_Class; Decl: Iir_Configuration_Declaration) is
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Configuration);
      Disp_Name_Of (Ctxt, Decl);
      Disp_Token (Ctxt, Tok_Of);
      Print (Ctxt, Get_Entity_Name (Decl));
      Disp_Token (Ctxt, Tok_Is);
      Close_Hbox (Ctxt);

      Start_Vbox (Ctxt);
      Disp_Declaration_Chain (Ctxt, Decl);
      Disp_Block_Configuration (Ctxt, Get_Block_Configuration (Decl));
      Close_Vbox (Ctxt);

      Disp_End (Ctxt, Decl, Tok_Configuration);
   end Disp_Configuration_Declaration;

   procedure Disp_Context_Items (Ctxt : in out Ctxt_Class; First : Iir)
   is
      Decl: Iir;
      Next_Decl : Iir;
   begin
      Decl := First;
      while Decl /= Null_Iir loop
         Next_Decl := Get_Chain (Decl);

         case Iir_Kinds_Clause (Get_Kind (Decl)) is
            when Iir_Kind_Use_Clause =>
               Disp_Use_Clause (Ctxt, Decl);
            when Iir_Kind_Library_Clause =>
               Start_Hbox (Ctxt);
               Disp_Token (Ctxt, Tok_Library);
               Disp_Identifier (Ctxt, Decl);
               while Get_Has_Identifier_List (Decl) loop
                  Decl := Next_Decl;
                  Next_Decl := Get_Chain (Decl);
                  Disp_Token (Ctxt, Tok_Comma);
                  Disp_Identifier (Ctxt, Decl);
               end loop;
               Disp_Token (Ctxt, Tok_Semi_Colon);
               Close_Hbox (Ctxt);
            when Iir_Kind_Context_Reference =>
               Start_Hbox (Ctxt);
               Disp_Token (Ctxt, Tok_Context);
               declare
                  Ref : Iir;
               begin
                  Ref := Decl;
                  loop
                     Print (Ctxt, Get_Selected_Name (Ref));
                     Ref := Get_Context_Reference_Chain (Ref);
                     exit when Ref = Null_Iir;
                     Disp_Token (Ctxt, Tok_Comma);
                  end loop;
                  Disp_Token (Ctxt, Tok_Semi_Colon);
               end;
               Close_Hbox (Ctxt);
         end case;
         Decl := Next_Decl;
      end loop;
   end Disp_Context_Items;

   procedure Disp_Context_Declaration (Ctxt : in out Ctxt_Class; Decl: Iir) is
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Context);
      Disp_Name_Of (Ctxt, Decl);
      Disp_Token (Ctxt, Tok_Is);
      Close_Hbox (Ctxt);
      Start_Vbox (Ctxt);
      Disp_Context_Items (Ctxt, Get_Context_Items (Decl));
      Close_Vbox (Ctxt);
      Disp_End (Ctxt, Decl, Tok_Context);
   end Disp_Context_Declaration;

   procedure Disp_Verification_Unit
     (Ctxt : in out Ctxt_Class; Unit: Iir; Tok : Token_Type)
   is
      Hier_Name : Iir;
      Arch : Iir;
      Item : Iir;
   begin
      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok);
      Disp_Name_Of (Ctxt, Unit);

      Hier_Name := Get_Hierarchical_Name (Unit);
      if Hier_Name /= Null_Iir then
         Disp_Token (Ctxt, Tok_Left_Paren);
         Print (Ctxt, Get_Entity_Name (Hier_Name));
         Arch := Get_Architecture (Hier_Name);
         if Arch /= Null_Iir then
            Disp_Token (Ctxt, Tok_Left_Paren);
            Print (Ctxt, Arch);
            Disp_Token (Ctxt, Tok_Right_Paren);
         end if;
         Disp_Token (Ctxt, Tok_Right_Paren);
      end if;
      Close_Hbox (Ctxt);

      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Left_Curly);
      Close_Hbox (Ctxt);

      Start_Vbox (Ctxt);
      Item := Get_Vunit_Item_Chain (Unit);
      while Item /= Null_Iir loop
         Print (Ctxt, Item);
         Item := Get_Chain (Item);
      end loop;
      Close_Vbox (Ctxt);

      Start_Hbox (Ctxt);
      Disp_Token (Ctxt, Tok_Right_Curly);
      Close_Hbox (Ctxt);
   end Disp_Verification_Unit;

   procedure Disp_Design_Unit (Ctxt : in out Ctxt_Class; Unit: Iir_Design_Unit)
   is
      Decl: Iir;
   begin
      Disp_Context_Items (Ctxt, Get_Context_Items (Unit));

      Decl := Get_Library_Unit (Unit);
      case Iir_Kinds_Library_Unit (Get_Kind (Decl)) is
         when Iir_Kind_Entity_Declaration =>
            Disp_Entity_Declaration (Ctxt, Decl);
         when Iir_Kind_Architecture_Body =>
            Disp_Architecture_Body (Ctxt, Decl);
         when Iir_Kind_Package_Declaration =>
            Disp_Package_Declaration (Ctxt, Decl);
         when Iir_Kind_Package_Body =>
            Disp_Package_Body (Ctxt, Decl);
         when Iir_Kind_Package_Instantiation_Declaration =>
            Disp_Package_Instantiation_Declaration (Ctxt, Decl);
         when Iir_Kind_Configuration_Declaration =>
            Disp_Configuration_Declaration (Ctxt, Decl);
         when Iir_Kind_Context_Declaration =>
            Disp_Context_Declaration (Ctxt, Decl);
         when Iir_Kind_Vunit_Declaration =>
            Disp_Verification_Unit (Ctxt, Decl, Tok_Vunit);
         when Iir_Kind_Vmode_Declaration =>
            Disp_Verification_Unit (Ctxt, Decl, Tok_Vmode);
         when Iir_Kind_Vprop_Declaration =>
            Disp_Verification_Unit (Ctxt, Decl, Tok_Vprop);
         when Iir_Kind_Foreign_Module =>
            raise Internal_Error;
      end case;
   end Disp_Design_Unit;

   procedure Disp_Vhdl (Ctxt : in out Ctxt_Class; N : Iir) is
   begin
      case Get_Kind (N) is
         when Iir_Kind_Design_File =>
            declare
               Unit : Iir;
            begin
               Unit := Get_First_Design_Unit (N);
               while Unit /= Null_Iir loop
                  Disp_Vhdl (Ctxt, Unit);
                  Unit := Get_Chain (Unit);
               end loop;
            end;
         when Iir_Kind_Design_Unit =>
            Start_Node (Ctxt, N);
            Disp_Design_Unit (Ctxt, N);
         when Iir_Kind_Enumeration_Type_Definition =>
            Disp_Enumeration_Type_Definition (Ctxt, N);
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
            Disp_Concurrent_Conditional_Signal_Assignment (Ctxt, N);
         when Iir_Kinds_Dyadic_Operator =>
            Disp_Dyadic_Operator (Ctxt, N);
         when Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Object_Alias_Declaration =>
            Disp_Name_Of (Ctxt, N);
         when Iir_Kind_Enumeration_Literal =>
            Disp_Identifier (Ctxt, N);
         when Iir_Kind_Component_Instantiation_Statement =>
            Disp_Component_Instantiation_Statement (Ctxt, N);
         when Iir_Kind_Array_Type_Definition =>
            Disp_Array_Type_Definition (Ctxt, N);
         when Iir_Kind_Package_Declaration =>
            Disp_Package_Declaration (Ctxt, N);
         when Iir_Kind_Wait_Statement =>
            Disp_Wait_Statement (Ctxt, N);
         when Iir_Kind_Selected_Name
           | Iir_Kind_Selected_Element
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Slice_Name =>
            Print (Ctxt, N);
         when Iir_Kind_Psl_Cover_Directive =>
            Disp_Psl_Cover_Directive (Ctxt, N);
         when others =>
            Error_Kind ("disp", N);
      end case;
   end Disp_Vhdl;

   procedure Print_Qualified_Expression (Ctxt : in out Ctxt_Class; Expr: Iir)
   is
      Qexpr : constant Iir := Strip_Literal_Origin (Get_Expression (Expr));
      Has_Paren : constant Boolean :=
        Get_Kind (Qexpr) = Iir_Kind_Parenthesis_Expression
        or else Get_Kind (Qexpr) = Iir_Kind_Aggregate;
   begin
      Print (Ctxt, Get_Type_Mark (Expr));
      Disp_Token (Ctxt, Tok_Tick);
      if not Has_Paren then
         Disp_Token (Ctxt, Tok_Left_Paren);
      end if;
      Print (Ctxt, Qexpr);
      if not Has_Paren then
         Disp_Token (Ctxt, Tok_Right_Paren);
      end if;
   end Print_Qualified_Expression;

   procedure Print (Ctxt : in out Ctxt_Class; Expr: Iir)
   is
      Orig : Iir;
   begin
      case Get_Kind (Expr) is
         when Iir_Kind_Integer_Literal =>
            Orig := Get_Literal_Origin (Expr);
            if Dump_Origin_Flag and then Orig /= Null_Iir then
               Print (Ctxt, Orig);
            else
               if Get_Literal_Length (Expr) /= 0 then
                  Disp_Literal_From_Source (Ctxt, Expr, Tok_Integer);
               else
                  Disp_Int64 (Ctxt, Get_Value (Expr));
               end if;
            end if;
         when Iir_Kind_Floating_Point_Literal =>
            Orig := Get_Literal_Origin (Expr);
            if Dump_Origin_Flag and then Orig /= Null_Iir then
               Print (Ctxt, Orig);
            else
               if Get_Literal_Length (Expr) /= 0 then
                  Disp_Literal_From_Source (Ctxt, Expr, Tok_Real);
               else
                  Disp_Fp64 (Ctxt, Get_Fp_Value (Expr));
               end if;
            end if;
         when Iir_Kind_String_Literal8 =>
            Orig := Get_Literal_Origin (Expr);
            if Dump_Origin_Flag and then Orig /= Null_Iir then
               Print (Ctxt, Orig);
            else
               declare
                  Expr_Type : constant Iir := Get_Type (Expr);
                  El_Type : Iir;
               begin
                  if Expr_Type /= Null_Iir then
                     El_Type := Get_Element_Subtype (Expr_Type);
                  else
                     El_Type := Null_Iir;
                  end if;
                  Disp_String_Literal (Ctxt, Expr, El_Type);
                  if Flag_Disp_String_Literal_Type or Flags.List_Verbose then
                     OOB.Put ("[type: ");
                     Disp_Type (Ctxt, Expr_Type);
                     OOB.Put ("]");
                  end if;
               end;
            end if;
         when Iir_Kind_Physical_Fp_Literal
           | Iir_Kind_Physical_Int_Literal =>
            Orig := Get_Literal_Origin (Expr);
            if Dump_Origin_Flag and then Orig /= Null_Iir then
               Print (Ctxt, Orig);
            else
               Disp_Physical_Literal (Ctxt, Expr);
            end if;
         when Iir_Kind_Enumeration_Literal =>
            Orig := Get_Literal_Origin (Expr);
            if Dump_Origin_Flag and then Orig /= Null_Iir then
               Print (Ctxt, Orig);
            else
               Disp_Name_Of (Ctxt, Expr);
            end if;
         when Iir_Kind_Overflow_Literal =>
            Orig := Get_Literal_Origin (Expr);
            if Dump_Origin_Flag and then Orig /= Null_Iir then
               Print (Ctxt, Orig);
            else
               Start_Lit (Ctxt, Tok_Integer);
               Disp_Str (Ctxt, "*OVERFLOW*");
               Close_Lit (Ctxt);
            end if;

         when Iir_Kind_Object_Alias_Declaration =>
            Disp_Name_Of (Ctxt, Expr);
         when Iir_Kind_Aggregate =>
            Disp_Aggregate (Ctxt, Expr);
         when Iir_Kind_Null_Literal =>
            Disp_Token (Ctxt, Tok_Null);
         when Iir_Kind_Simple_Aggregate =>
            Orig := Get_Literal_Origin (Expr);
            if Dump_Origin_Flag and then Orig /= Null_Iir then
               Print (Ctxt, Orig);
            else
               Disp_Simple_Aggregate (Ctxt, Expr);
            end if;

         when Iir_Kind_Attribute_Value =>
            Disp_Attribute_Value (Ctxt, Expr);
         when Iir_Kind_Attribute_Name =>
            Disp_Attribute_Name (Ctxt, Expr);

         when Iir_Kind_Element_Declaration =>
            Disp_Name_Of (Ctxt, Expr);

         when Iir_Kind_Signal_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Iterator_Declaration =>
            Disp_Name_Of (Ctxt, Expr);
            return;
         when Iir_Kind_Reference_Name =>
            declare
               Name : constant Iir := Get_Referenced_Name (Expr);
            begin
               if Is_Valid (Name) then
                  Print (Ctxt, Name);
               else
                  Print (Ctxt, Get_Named_Entity (Expr));
               end if;
            end;

         when Iir_Kinds_Dyadic_Operator =>
            Disp_Dyadic_Operator (Ctxt, Expr);
         when Iir_Kinds_Monadic_Operator =>
            Disp_Monadic_Operator (Ctxt, Expr);
         when Iir_Kind_Function_Call =>
            Disp_Function_Call (Ctxt, Expr);
         when Iir_Kind_Parenthesis_Expression =>
            Disp_Token (Ctxt, Tok_Left_Paren);
            Print (Ctxt, Get_Expression (Expr));
            Disp_Token (Ctxt, Tok_Right_Paren);
         when Iir_Kind_Type_Conversion =>
            Print (Ctxt, Get_Type_Mark (Expr));
            Disp_Token (Ctxt, Tok_Left_Paren);
            Print (Ctxt, Get_Expression (Expr));
            Disp_Token (Ctxt, Tok_Right_Paren);
         when Iir_Kind_Qualified_Expression =>
            Print_Qualified_Expression (Ctxt, Expr);
         when Iir_Kind_Allocator_By_Expression =>
            Disp_Token (Ctxt, Tok_New);
            Print (Ctxt, Get_Expression (Expr));
         when Iir_Kind_Allocator_By_Subtype =>
            Disp_Token (Ctxt, Tok_New);
            Disp_Subtype_Indication (Ctxt, Get_Subtype_Indication (Expr));

         when Iir_Kind_Indexed_Name =>
            Disp_Indexed_Name (Ctxt, Expr);
         when Iir_Kind_Slice_Name =>
            Print (Ctxt, Get_Prefix (Expr));
            Disp_Token (Ctxt, Tok_Left_Paren);
            Disp_Range (Ctxt, Get_Suffix (Expr));
            Disp_Token (Ctxt, Tok_Right_Paren);
         when Iir_Kind_Selected_Element =>
            Print (Ctxt, Get_Prefix (Expr));
            Disp_Token (Ctxt, Tok_Dot);
            Disp_Name_Of (Ctxt, Get_Named_Entity (Expr));
         when Iir_Kind_Implicit_Dereference =>
            Print (Ctxt, Get_Prefix (Expr));

         when Iir_Kind_Left_Type_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_Left);
         when Iir_Kind_Right_Type_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_Right);
         when Iir_Kind_High_Type_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_High);
         when Iir_Kind_Low_Type_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_Low);
         when Iir_Kind_Ascending_Type_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_Ascending);

         when Iir_Kind_Nature_Reference_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_Reference);
         when Iir_Kind_Across_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_Across);
         when Iir_Kind_Through_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_Through);

         when Iir_Kind_Dot_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_Dot);
         when Iir_Kind_Integ_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_Integ);
         when Iir_Kind_Zoh_Attribute =>
            Disp_Parametered_Attribute (Ctxt, Name_Zoh, Expr, 2);
         when Iir_Kind_Ltf_Attribute =>
            Disp_Parametered_Attribute (Ctxt, Name_Ltf, Expr, 2);
         when Iir_Kind_Ztf_Attribute =>
            Disp_Parametered_Attribute (Ctxt, Name_Ztf, Expr, 4);

         when Iir_Kind_Signal_Slew_Attribute
           | Iir_Kind_Quantity_Slew_Attribute =>
            Disp_Parametered_Attribute (Ctxt, Name_Slew, Expr, 2);
         when Iir_Kind_Quantity_Delayed_Attribute =>
            Disp_Parametered_Attribute (Ctxt, Name_Delayed, Expr, 1);
         when Iir_Kind_Ramp_Attribute =>
            Disp_Parametered_Attribute (Ctxt, Name_Ramp, Expr, 2);
         when Iir_Kind_Above_Attribute =>
            Disp_Parametered_Attribute (Ctxt, Name_Above, Expr);
         when Iir_Kind_Stable_Attribute =>
            Disp_Parametered_Attribute (Ctxt, Name_Stable, Expr);
         when Iir_Kind_Quiet_Attribute =>
            Disp_Parametered_Attribute (Ctxt, Name_Quiet, Expr);
         when Iir_Kind_Delayed_Attribute =>
            Disp_Parametered_Attribute (Ctxt, Name_Delayed, Expr);
         when Iir_Kind_Transaction_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_Transaction);
         when Iir_Kind_Event_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_Event);
         when Iir_Kind_Active_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_Active);
         when Iir_Kind_Driving_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_Driving);
         when Iir_Kind_Driving_Value_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_Driving_Value);
         when Iir_Kind_Last_Value_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_Last_Value);
         when Iir_Kind_Last_Active_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_Last_Active);
         when Iir_Kind_Last_Event_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_Last_Event);

         when Iir_Kind_Pos_Attribute =>
            Disp_Parametered_Type_Attribute (Ctxt, Name_Pos, Expr);
         when Iir_Kind_Val_Attribute =>
            Disp_Parametered_Type_Attribute (Ctxt, Name_Val, Expr);
         when Iir_Kind_Succ_Attribute =>
            Disp_Parametered_Type_Attribute (Ctxt, Name_Succ, Expr);
         when Iir_Kind_Pred_Attribute =>
            Disp_Parametered_Type_Attribute (Ctxt, Name_Pred, Expr);
         when Iir_Kind_Leftof_Attribute =>
            Disp_Parametered_Type_Attribute (Ctxt, Name_Leftof, Expr);
         when Iir_Kind_Rightof_Attribute =>
            Disp_Parametered_Type_Attribute (Ctxt, Name_Rightof, Expr);

         when Iir_Kind_Length_Array_Attribute =>
            Disp_Parametered_Attribute (Ctxt, Name_Length, Expr);
         when Iir_Kind_Range_Array_Attribute =>
            Disp_Parametered_Attribute (Ctxt, Name_Range, Expr);
         when Iir_Kind_Reverse_Range_Array_Attribute =>
            Disp_Parametered_Attribute (Ctxt, Name_Reverse_Range, Expr);
         when Iir_Kind_Left_Array_Attribute =>
            Disp_Parametered_Attribute (Ctxt, Name_Left, Expr);
         when Iir_Kind_Right_Array_Attribute =>
            Disp_Parametered_Attribute (Ctxt, Name_Right, Expr);
         when Iir_Kind_Low_Array_Attribute =>
            Disp_Parametered_Attribute (Ctxt, Name_Low, Expr);
         when Iir_Kind_High_Array_Attribute =>
            Disp_Parametered_Attribute (Ctxt, Name_High, Expr);
         when Iir_Kind_Ascending_Array_Attribute =>
            Disp_Parametered_Attribute (Ctxt, Name_Ascending, Expr);

         when Iir_Kind_Image_Attribute =>
            Disp_Parametered_Attribute (Ctxt, Name_Image, Expr);
         when Iir_Kind_Value_Attribute =>
            Disp_Parametered_Attribute (Ctxt, Name_Value, Expr);
         when Iir_Kind_Simple_Name_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_Simple_Name);
         when Iir_Kind_Instance_Name_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_Instance_Name);
         when Iir_Kind_Path_Name_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_Path_Name);

         when Iir_Kind_Psl_Prev =>
            Disp_Psl_Prev (Ctxt, Expr);
         when Iir_Kind_Psl_Stable =>
            Disp_Psl_Stable (Ctxt, Expr);
         when Iir_Kind_Psl_Rose =>
            Disp_Psl_Rose (Ctxt, Expr);
         when Iir_Kind_Psl_Fell =>
            Disp_Psl_Fell (Ctxt, Expr);
         when Iir_Kind_Psl_Onehot =>
            Disp_Psl_Onehot (Ctxt, Expr);
         when Iir_Kind_Psl_Onehot0 =>
            Disp_Psl_Onehot0 (Ctxt, Expr);

         when Iir_Kinds_Type_And_Subtype_Definition =>
            Disp_Type (Ctxt, Expr);

         when Iir_Kind_Range_Expression =>
            Disp_Range (Ctxt, Expr);
         when Iir_Kind_Subtype_Definition =>
            Disp_Subtype_Indication (Ctxt, Expr);

         when Iir_Kind_Selected_By_All_Name
           | Iir_Kind_Dereference =>
            Print (Ctxt, Get_Prefix (Expr));
            Disp_Token (Ctxt, Tok_Dot, Tok_All);
         when Iir_Kind_Simple_Name
           | Iir_Kind_Character_Literal =>
            Disp_Identifier (Ctxt, Expr);
         when Iir_Kind_Operator_Symbol =>
            Disp_Function_Name (Ctxt, Expr);
         when Iir_Kind_Selected_Name =>
            Print (Ctxt, Get_Prefix (Expr));
            Disp_Token (Ctxt, Tok_Dot);
            Disp_Function_Name (Ctxt, Expr);
         when Iir_Kind_Parenthesis_Name =>
            Print (Ctxt, Get_Prefix (Expr));
            Disp_Association_Chain (Ctxt, Get_Association_Chain (Expr));
         when Iir_Kind_Base_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_Base);
         when Iir_Kind_Subtype_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_Subtype);
         when Iir_Kind_Element_Attribute =>
            Disp_Name_Attribute (Ctxt, Expr, Name_Element);
         when Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Unit_Declaration
           | Iir_Kinds_Interface_Object_Declaration
           | Iir_Kinds_Interface_Subprogram_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Terminal_Declaration
           | Iir_Kinds_Quantity_Declaration
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Psl_Endpoint_Declaration =>
            Disp_Name_Of (Ctxt, Expr);

         when Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body =>
            Disp_Subprogram_Body (Ctxt, Expr);

         when Iir_Kind_Attribute_Declaration =>
            Disp_Attribute_Declaration (Ctxt, Expr);
         when Iir_Kind_Attribute_Specification =>
            Disp_Attribute_Specification (Ctxt, Expr);

         when Iir_Kind_Signature =>
            Disp_Signature (Ctxt, Expr);

         when Iir_Kind_Psl_Default_Clock =>
            Disp_Psl_Default_Clock (Ctxt, Expr);
         when Iir_Kind_Psl_Assert_Directive =>
            Disp_Psl_Assert_Directive (Ctxt, Expr);
         when Iir_Kind_Psl_Assume_Directive =>
            Disp_Psl_Assume_Directive (Ctxt, Expr);
         when Iir_Kind_Psl_Restrict_Directive =>
            Disp_Psl_Restrict_Directive (Ctxt, Expr);
         when Iir_Kind_Psl_Boolean_Parameter =>
            Disp_Name_Of (Ctxt, Expr);

         when Iir_Kind_Error =>
            declare
               Orig : constant Iir := Get_Error_Origin (Expr);
            begin
               if Orig /= Null_Iir then
                  Print (Ctxt, Orig);
               else
                  Error_Kind ("print/error", Expr);
               end if;
            end;
         when others =>
            Error_Kind ("print", Expr);
      end case;
   end Print;

   procedure Disp_Int_Trim (Ctxt : in out Ctxt_Class; Str : String) is
   begin
      Start_Lit (Ctxt, Tok_Integer);
      if Str (Str'First) = ' ' then
         Disp_Str (Ctxt, Str (Str'First + 1 .. Str'Last));
      else
         Disp_Str (Ctxt, Str);
      end if;
      Close_Lit (Ctxt);
   end Disp_Int_Trim;

   procedure Disp_Int64 (Ctxt : in out Ctxt_Class; Val: Int64) is
   begin
      Disp_Int_Trim (Ctxt, Int64'Image (Val));
   end Disp_Int64;

   procedure Disp_Int32 (Ctxt : in out Ctxt_Class; Val: Iir_Int32) is
   begin
      Disp_Int_Trim (Ctxt, Iir_Int32'Image (Val));
   end Disp_Int32;

   procedure Disp_Fp64 (Ctxt : in out Ctxt_Class; Val: Fp64)
   is
      Str: constant String := Fp64'Image (Val);
   begin
      Start_Lit (Ctxt, Tok_Real);
      if Str (Str'First) = ' ' then
         Disp_Str (Ctxt, Str (Str'First + 1 .. Str'Last));
      else
         Disp_Str (Ctxt, Str);
      end if;
      Close_Lit (Ctxt);
   end Disp_Fp64;

   procedure Disp_Str (Ctxt : in out Ctxt_Class; Str : String) is
   begin
      for I in Str'Range loop
         Disp_Char (Ctxt, Str (I));
      end loop;
   end Disp_Str;

   function Need_Space (Tok, Prev_Tok : Token_Type) return Boolean is
   begin
      if Prev_Tok = Tok_Newline then
         return False;
      elsif Prev_Tok >= Tok_First_Keyword then
         --  A space after a keyword.
         if Tok /= Tok_Semi_Colon
           and Tok /= Tok_Dot
           and Tok /= Tok_Right_Paren
         then
            return True;
         end if;
      elsif Tok >= Tok_First_Keyword then
         --  Space before a keyword.
         if Prev_Tok /= Tok_Dot
           and Prev_Tok /= Tok_Left_Paren
         then
            return True;
         end if;
      elsif (Tok = Tok_Identifier
               or Tok = Tok_String)
        and (Prev_Tok = Tok_Identifier
               or Prev_Tok = Tok_String
               or Prev_Tok = Tok_Integer
               or Prev_Tok = Tok_Real)
      then
         --  A space is needed between 2 identifiers.
         return True;
      elsif Prev_Tok = Tok_Comma
        or Prev_Tok = Tok_Semi_Colon
        or Prev_Tok = Tok_Colon
        or Prev_Tok = Tok_Assign
        or Prev_Tok = Tok_Double_Arrow
        or Prev_Tok = Tok_Equal_Equal
        or Prev_Tok in Token_Relational_Operator_Type
        or Prev_Tok in Token_Adding_Operator_Type
        or Prev_Tok in Token_Multiplying_Operator_Type
        or Prev_Tok = Tok_Bar
      then
         --  Always a space after ',', ':', ':='
         return True;
      elsif Tok = Tok_Left_Paren then
         if Prev_Tok /= Tok_Tick
           and Prev_Tok /= Tok_Left_Paren
           and Prev_Tok /= Tok_Right_Paren
           and Prev_Tok /= Tok_Exclam_Mark
         then
            --  A space before '('.
            return True;
         end if;
      elsif Tok = Tok_Left_Bracket
        or Tok = Tok_Assign
        or Tok = Tok_Double_Arrow
        or Tok = Tok_Equal_Equal
        or Tok in Token_Relational_Operator_Type
        or Tok in Token_Adding_Operator_Type
        or Tok in Token_Multiplying_Operator_Type
        or Tok = Tok_Minus_Greater
        or Tok = Tok_Bar
      then
         --  Always a space before '[', ':='.
         return True;
      end if;
      return False;
   end Need_Space;

   package Simple_Disp_Ctxt is
      type Simple_Ctxt is new Disp_Ctxt with record
         --  Boxes level.
         Vnum : Natural;
         Hnum : Natural;

         --  Used by comments.
         Sfe : Source_File_Entry;

         --  Previous token, to decided whether or not a blank must be added.
         Prev_Tok : Token_Type;
      end record;

      procedure Init (Ctxt : out Simple_Ctxt);
      procedure Start_Hbox (Ctxt : in out Simple_Ctxt);
      procedure Close_Hbox (Ctxt : in out Simple_Ctxt);
      procedure Start_Vbox (Ctxt : in out Simple_Ctxt);
      procedure Close_Vbox (Ctxt : in out Simple_Ctxt);
      procedure Start_Node (Ctxt : in out Simple_Ctxt; N : Iir);
      procedure Valign (Ctxt : in out Simple_Ctxt; Point : Valign_Type);
      procedure Disp_Token (Ctxt : in out Simple_Ctxt; Tok : Token_Type);
      procedure Start_Lit (Ctxt : in out Simple_Ctxt; Tok : Token_Type);
      procedure Disp_Char (Ctxt : in out Simple_Ctxt; C : Character);
      procedure Close_Lit (Ctxt : in out Simple_Ctxt);
   private
      procedure Put (Ctxt : in out Simple_Ctxt; C : Character);
   end Simple_Disp_Ctxt;

   package body Simple_Disp_Ctxt is
      procedure Init (Ctxt : out Simple_Ctxt) is
      begin
         Ctxt := (Vnum => 0,
                  Hnum => 0,
                  Sfe => No_Source_File_Entry,
                  Prev_Tok => Tok_Newline);
      end Init;

      procedure Put (Ctxt : in out Simple_Ctxt; C : Character)
      is
         pragma Unreferenced (Ctxt);
      begin
         Simple_IO.Put (C);
      end Put;

      procedure Start_Hbox (Ctxt : in out Simple_Ctxt) is
      begin
         if Ctxt.Hnum = 0 then
            for I in 1 .. Ctxt.Vnum loop
               Put (Ctxt, ' ');
               Put (Ctxt, ' ');
            end loop;
         end if;
         Ctxt.Hnum := Ctxt.Hnum + 1;
      end Start_Hbox;

      procedure Close_Hbox (Ctxt : in out Simple_Ctxt) is
      begin
         Ctxt.Hnum := Ctxt.Hnum - 1;
         if Ctxt.Hnum = 0 then
            Put (Ctxt, ASCII.LF);
            Ctxt.Prev_Tok := Tok_Newline;
         end if;
      end Close_Hbox;

      procedure Start_Vbox (Ctxt : in out Simple_Ctxt) is
      begin
         pragma Assert (Ctxt.Hnum = 0);
         Ctxt.Vnum := Ctxt.Vnum + 1;
      end Start_Vbox;

      procedure Close_Vbox (Ctxt : in out Simple_Ctxt) is
      begin
         Ctxt.Vnum := Ctxt.Vnum - 1;
      end Close_Vbox;

      procedure Start_Node (Ctxt : in out Simple_Ctxt; N : Iir)
      is
         use File_Comments;
         Sfe : Source_File_Entry;
         Idx : Comment_Index;
      begin
         if not Flag_Gather_Comments then
            return;
         end if;
         Sfe := Ctxt.Sfe;
         if Sfe = No_Source_File_Entry then
            Sfe := Files_Map.Location_To_File (Get_Location (N));
            Ctxt.Sfe := Sfe;
         end if;
         Idx := Find_First_Comment (Sfe, Uns32 (N));
         while Idx /= No_Comment_Index loop
            declare
               Buf : constant File_Buffer_Acc :=
                 Files_Map.Get_File_Source (Sfe);
               Start, Last : Source_Ptr;
            begin
               --  TODO: indent
               Get_Comment (Sfe, Idx, Start, Last);
               Start_Hbox (Ctxt);
               for I in Start .. Last loop
                  Disp_Char (Ctxt, Buf (I));
               end loop;
               Close_Hbox (Ctxt);
            end;
            Idx := Get_Next_Comment (Sfe, Idx);
         end loop;
      end Start_Node;

      procedure Valign (Ctxt : in out Simple_Ctxt; Point : Valign_Type) is
      begin
         null;
      end Valign;

      procedure Disp_Space (Ctxt : in out Simple_Ctxt; Tok : Token_Type)
      is
         Prev_Tok : constant Token_Type := Ctxt.Prev_Tok;
      begin
         if Need_Space (Tok, Prev_Tok) then
            Put (Ctxt, ' ');
         end if;
         Ctxt.Prev_Tok := Tok;
      end Disp_Space;

      procedure Disp_Token (Ctxt : in out Simple_Ctxt; Tok : Token_Type) is
      begin
         Disp_Space (Ctxt, Tok);
         Disp_Str (Ctxt, Image (Tok));
      end Disp_Token;

      procedure Start_Lit (Ctxt : in out Simple_Ctxt; Tok : Token_Type) is
      begin
         Disp_Space (Ctxt, Tok);
      end Start_Lit;

      procedure Disp_Char (Ctxt : in out Simple_Ctxt; C : Character) is
      begin
         Put (Ctxt, C);
      end Disp_Char;

      procedure Close_Lit (Ctxt : in out Simple_Ctxt) is
      begin
         null;
      end Close_Lit;
   end Simple_Disp_Ctxt;

   procedure Disp_Vhdl (N : Iir)
   is
      use Simple_Disp_Ctxt;
      Ctxt : Simple_Ctxt;
   begin
      Init (Ctxt);
      Disp_Vhdl (Ctxt, N);
   end Disp_Vhdl;

   procedure Disp_Expression (Expr: Iir)
   is
      use Simple_Disp_Ctxt;
      Ctxt : Simple_Ctxt;
   begin
      Init (Ctxt);
      Print (Ctxt, Expr);
   end Disp_Expression;

   procedure Disp_PSL_NFA (N : PSL.Nodes.NFA)
   is
      use Simple_Disp_Ctxt;
      Ctxt : Simple_Ctxt;
   begin
      Init (Ctxt);
      Disp_PSL_NFA (Ctxt, N);
   end Disp_PSL_NFA;

   procedure Disp_PSL_Expr (N : PSL_Node)
   is
      use Simple_Disp_Ctxt;
      Ctxt : Simple_Ctxt;
   begin
      Init (Ctxt);
      Disp_Psl_Expression (Ctxt, N);
      OOB.New_Line;
   end Disp_PSL_Expr;

end Vhdl.Prints;
