--  Error message handling.
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
with Ada.Text_IO;
with Ada.Command_Line;
with Scan;
with Tokens; use Tokens;
with Name_Table;
with Iirs_Utils;
with Files_Map; use Files_Map;
with Ada.Strings.Unbounded;
with Std_Names;
with Flags;
with PSL.Nodes;

package body Errorout is
   procedure Put (Str : String)
   is
      use Ada.Text_IO;
   begin
      Put (Standard_Error, Str);
   end Put;

   procedure Put (C : Character)
   is
      use Ada.Text_IO;
   begin
      Put (Standard_Error, C);
   end Put;

   procedure Put_Line (Str : String)
   is
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, Str);
   end Put_Line;

   procedure Disp_Natural (Val: Natural)
   is
      Str: constant String := Natural'Image (Val);
   begin
      Put (Str(Str'First + 1 .. Str'Last));
   end Disp_Natural;

   procedure Error_Msg (Msg: String) is
   begin
      Put (Ada.Command_Line.Command_Name);
      Put (": ");
      Put_Line (Msg);
   end Error_Msg;

   procedure Error_Kind (Msg : String; An_Iir : Iir) is
   begin
      Put_Line (Msg & ": cannot handle "
                & Iir_Kind'Image (Get_Kind (An_Iir))
                & " (" & Disp_Location (An_Iir) & ')');
      raise Internal_Error;
   end Error_Kind;

   procedure Error_Kind (Msg : String; Def : Iir_Predefined_Functions) is
   begin
      Put_Line (Msg & ": cannot handle "
                & Iir_Predefined_Functions'Image (Def));
      raise Internal_Error;
   end Error_Kind;

   procedure Error_Kind (Msg : String; N : PSL_Node) is
   begin
      Put (Msg);
      Put (": cannot handle ");
      Put_Line (PSL.Nodes.Nkind'Image (PSL.Nodes.Get_Kind (N)));
      raise Internal_Error;
   end Error_Kind;

   procedure Error_Msg_Option_NR (Msg: String) is
   begin
      Put (Ada.Command_Line.Command_Name);
      Put (": ");
      Put_Line (Msg);
   end Error_Msg_Option_NR;

   procedure Error_Msg_Option (Msg: String) is
   begin
      Error_Msg_Option_NR (Msg);
      raise Option_Error;
   end Error_Msg_Option;

   procedure Disp_Location
     (File: Name_Id; Line: Natural; Col: Natural) is
   begin
      Put (Name_Table.Image (File));
      Put (':');
      Disp_Natural (Line);
      Put (':');
      Disp_Natural (Col);
      Put (':');
   end Disp_Location;

   procedure Disp_Current_Location is
   begin
      Disp_Location (Scan.Get_Current_File,
                     Scan.Get_Current_Line,
                     Scan.Get_Current_Column);
   end Disp_Current_Location;

   procedure Disp_Token_Location is
   begin
      Disp_Location (Scan.Get_Current_File,
                     Scan.Get_Current_Line,
                     Scan.Get_Token_Column);
   end Disp_Token_Location;

   procedure Disp_Location (Loc : Location_Type)
   is
      Name : Name_Id;
      Line : Natural;
      Col : Natural;
   begin
      if Loc = Location_Nil then
         --  Avoid a crash, but should not happen.
         Put ("??:??:??:");
      else
         Location_To_Position (Loc, Name, Line, Col);
         Disp_Location (Name, Line, Col);
      end if;
   end Disp_Location;

   function Get_Location_Safe (N : Iir) return Location_Type is
   begin
      if N = Null_Iir then
         return Location_Nil;
      else
         return Get_Location (N);
      end if;
   end Get_Location_Safe;

   procedure Disp_Iir_Location (An_Iir: Iir) is
   begin
      Disp_Location (Get_Location_Safe (An_Iir));
   end Disp_Iir_Location;

   procedure Disp_PSL_Location (N : PSL_Node) is
   begin
      Disp_Location (PSL.Nodes.Get_Location (N));
   end Disp_PSL_Location;

   procedure Warning_Msg (Msg: String) is
   begin
      Put ("warning: ");
      Put_Line (Msg);
   end Warning_Msg;

   procedure Warning_Msg_Parse (Msg: String) is
   begin
      if Flags.Flag_Only_Elab_Warnings then
         return;
      end if;
      Disp_Token_Location;
      if Flags.Warn_Error then
         Nbr_Errors := Nbr_Errors + 1;
         Put (" ");
      else
         Put ("warning: ");
      end if;
      Put_Line (Msg);
   end Warning_Msg_Parse;

   procedure Warning_Msg_Sem (Msg: String; Loc : Location_Type) is
   begin
      if Flags.Flag_Only_Elab_Warnings then
         return;
      end if;
      Disp_Location (Loc);
      if Flags.Warn_Error then
         Nbr_Errors := Nbr_Errors + 1;
         Put (" ");
      else
         Put ("warning: ");
      end if;
      Put_Line (Msg);
   end Warning_Msg_Sem;

   procedure Warning_Msg_Sem (Msg: String; Loc : Iir) is
   begin
      Warning_Msg_Sem (Msg, Get_Location_Safe (Loc));
   end Warning_Msg_Sem;

   procedure Warning_Msg_Elab (Msg: String; Loc : Location_Type) is
   begin
      Disp_Location (Loc);
      if Flags.Warn_Error then
         Nbr_Errors := Nbr_Errors + 1;
         Put (" ");
      else
         Put ("warning: ");
      end if;
      Put_Line (Msg);
   end Warning_Msg_Elab;

   procedure Warning_Msg_Elab (Msg: String; Loc : Iir) is
   begin
      Warning_Msg_Elab (Msg, Get_Location_Safe (Loc));
   end Warning_Msg_Elab;

   procedure Disp_Current_Token;
   pragma Unreferenced (Disp_Current_Token);

   procedure Disp_Current_Token is
   begin
      case Scan.Current_Token is
         when Tok_Identifier =>
            Put ("identifier """
                 & Name_Table.Image (Scan.Current_Identifier) & """");
         when others =>
            Put (Token_Type'Image (Scan.Current_Token));
      end case;
   end Disp_Current_Token;

   -- Disp a message during scan.
   procedure Error_Msg_Scan (Msg: String) is
   begin
      Nbr_Errors := Nbr_Errors + 1;
      Disp_Current_Location;
      Put (' ');
      Put_Line (Msg);
   end Error_Msg_Scan;

   -- Disp a message during scan.
   procedure Warning_Msg_Scan (Msg: String) is
   begin
      Disp_Current_Location;
      Put ("warning: ");
      Put_Line (Msg);
   end Warning_Msg_Scan;

   -- Disp a message during scan.
   procedure Error_Msg_Parse (Msg: String) is
   begin
      Nbr_Errors := Nbr_Errors + 1;
      Disp_Token_Location;
      Put (' ');
      Put_Line (Msg);
   end Error_Msg_Parse;

   procedure Error_Msg_Parse (Msg: String; Loc : Iir) is
   begin
      Nbr_Errors := Nbr_Errors + 1;
      Disp_Iir_Location (Loc);
      Put (' ');
      Put_Line (Msg);
   end Error_Msg_Parse;

   procedure Error_Msg_Parse (Msg: String; Loc : Location_Type) is
   begin
      Nbr_Errors := Nbr_Errors + 1;
      Disp_Location (Loc);
      Put (' ');
      Put_Line (Msg);
   end Error_Msg_Parse;

   -- Disp a message during semantic analysis.
   -- LOC is used for location and current token.
   procedure Error_Msg_Sem (Msg: String; Loc: in Iir) is
   begin
      Nbr_Errors := Nbr_Errors + 1;
      if Loc /= Null_Iir then
         Disp_Iir_Location (Loc);
         Put (' ');
      end if;
      Put_Line (Msg);
   end Error_Msg_Sem;

   procedure Error_Msg_Sem (Msg: String; Loc: PSL_Node) is
      use PSL.Nodes;
   begin
      Nbr_Errors := Nbr_Errors + 1;
      if Loc /= Null_Node then
         Disp_PSL_Location (Loc);
         Put (' ');
      end if;
      Put_Line (Msg);
   end Error_Msg_Sem;

   procedure Error_Msg_Sem (Msg: String; Loc : Location_Type) is
   begin
      Nbr_Errors := Nbr_Errors + 1;
      Disp_Location (Loc);
      Put (' ');
      Put_Line (Msg);
   end Error_Msg_Sem;

   -- Disp a message during elaboration.
   procedure Error_Msg_Elab (Msg: String) is
   begin
      Nbr_Errors := Nbr_Errors + 1;
      Put ("error: ");
      Put_Line (Msg);
   end Error_Msg_Elab;

   procedure Error_Msg_Elab (Msg: String; Loc : Iir) is
   begin
      Nbr_Errors := Nbr_Errors + 1;
      Disp_Iir_Location (Loc);
      Put (' ');
      Put_Line (Msg);
   end Error_Msg_Elab;

   -- Disp a message during execution.
   procedure Error_Msg_Exec (Msg: String; Loc: in Iir) is
   begin
      Nbr_Errors := Nbr_Errors + 1;
      Disp_Iir_Location (Loc);
      Put (' ');
      Put_Line (Msg);
      raise Simulation_Error;
   end Error_Msg_Exec;

   procedure Warning_Msg_Exec (Msg: String; Loc: Iir) is
   begin
      Disp_Iir_Location (Loc);
      Put ("warning: ");
      Put_Line (Msg);
   end Warning_Msg_Exec;

   -- Disp a message for a constraint error.
   procedure Error_Msg_Constraint (Expr: in Iir) is
   begin
      Nbr_Errors := Nbr_Errors + 1;
      if Expr /= Null_Iir then
         Disp_Iir_Location (Expr);
      end if;
      Put ("constraint violation");
      if Expr /= Null_Iir then
         case Get_Kind (Expr) is
            when Iir_Kind_Addition_Operator =>
               Put_Line (" in the ""+"" operation");
            when Iir_Kind_Substraction_Operator =>
               Put_Line (" in the ""-"" operation");
            when Iir_Kind_Integer_Literal =>
               Put_Line (", literal out of range");
            when Iir_Kind_Signal_Interface_Declaration
              | Iir_Kind_Signal_Declaration =>
               Put_Line (" for " & Disp_Node (Expr));
            when others =>
               Put_Line ("");
         end case;
      end if;
      raise Execution_Constraint_Error;
   end Error_Msg_Constraint;

   -- Disp a bug message.
   procedure Error_Internal (Expr: in Iir; Msg: String := "")
   is
      pragma Unreferenced (Expr);
   begin
      Put ("internal error: ");
      Put_Line (Msg);
      raise Internal_Error;
   end Error_Internal;

   function Disp_Label (Node : Iir; Str : String) return String
   is
      Id : Name_Id;
   begin
      Id := Get_Label (Node);
      if Id = Null_Identifier then
         return "(unlabeled) " & Str;
      else
         return Str & " labeled """ & Name_Table.Image (Id) & """";
      end if;
   end Disp_Label;


   -- Disp a node.
   -- Used for output of message.
   function Disp_Node (Node: Iir) return String is
      function Disp_Identifier (Node : Iir; Str : String) return String
      is
         Id : Name_Id;
      begin
         Id := Get_Identifier (Node);
         return Str & " """ & Name_Table.Image (Id) & """";
      end Disp_Identifier;

      function Disp_Type (Node : Iir; Str : String) return String
      is
         Decl: Iir;
      begin
         Decl := Get_Type_Declarator (Node);
         if Decl = Null_Iir then
            return "the anonymous " & Str
              & " defined at " & Disp_Location (Node);
         else
            return Disp_Identifier (Decl, Str);
         end if;
      end Disp_Type;

   begin
      case Get_Kind (Node) is
         when Iir_Kind_String_Literal =>
            return "string literal """
              & Iirs_Utils.Image_String_Lit (Node) & """";
         when Iir_Kind_Bit_String_Literal =>
            return "bit string literal """
              & Iirs_Utils.Image_String_Lit (Node) & """";
         when Iir_Kind_Character_Literal =>
            return "character literal " & Iirs_Utils.Image_Identifier (Node);
         when Iir_Kind_Integer_Literal =>
            return "integer literal";
         when Iir_Kind_Floating_Point_Literal =>
            return "floating point literal";
         when Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal =>
            return "physical literal";
         when Iir_Kind_Enumeration_Literal =>
            return "enumeration literal " & Iirs_Utils.Image_Identifier (Node);
         when Iir_Kind_Element_Declaration =>
            return Disp_Identifier (Node, "element");
         when Iir_Kind_Record_Element_Constraint =>
            return "record element constraint";
         when Iir_Kind_Null_Literal =>
            return "null literal";
         when Iir_Kind_Aggregate =>
            return "aggregate";
         when Iir_Kind_Unit_Declaration =>
            return Disp_Identifier (Node, "physical unit");
         when Iir_Kind_Simple_Aggregate =>
            return "locally static array literal";

         --  Should never be displayed, but for completness...
         when Iir_Kind_Proxy =>
            return "proxy";
         when Iir_Kind_Operator_Symbol =>
            return "operator name";
         when Iir_Kind_Aggregate_Info =>
            return "aggregate info";
         when Iir_Kind_Signature =>
            return "signature";
         when Iir_Kind_Waveform_Element =>
            return "waveform element";
         when Iir_Kind_Conditional_Waveform =>
            return "conditional waveform";
         when Iir_Kind_Association_Element_Open =>
            return "open association element";
         when Iir_Kind_Association_Element_By_Individual =>
            return "individual association element";
         when Iir_Kind_Association_Element_By_Expression =>
            return "association element";
         when Iir_Kind_Overload_List =>
            return "overloaded name or expression";

         when Iir_Kind_Array_Type_Definition =>
            return Disp_Type (Node, "array type");
         when Iir_Kind_Array_Subtype_Definition =>
            return Disp_Type (Node, "array subtype");
         when Iir_Kind_Record_Type_Definition =>
            return Disp_Type (Node, "record type");
         when Iir_Kind_Record_Subtype_Definition =>
            return Disp_Type (Node, "record subtype");
         when Iir_Kind_Enumeration_Subtype_Definition =>
            return Disp_Type (Node, "enumeration subtype");
         when Iir_Kind_Integer_Subtype_Definition =>
            return Disp_Type (Node, "integer subtype");
         when Iir_Kind_Physical_Type_Definition =>
            return Disp_Type (Node, "physical type");
         when Iir_Kind_Physical_Subtype_Definition =>
            return Disp_Type (Node, "physical subtype");
         when Iir_Kind_File_Type_Definition =>
            return Disp_Type (Node, "file type");
         when Iir_Kind_Access_Type_Definition =>
            return Disp_Type (Node, "access type");
         when Iir_Kind_Access_Subtype_Definition =>
            return Disp_Type (Node, "access subtype");
         when Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Floating_Type_Definition =>
            return Disp_Type (Node, "floating type");
         when Iir_Kind_Incomplete_Type_Definition =>
            return Disp_Type (Node, "incomplete type");
         when Iir_Kind_Protected_Type_Declaration =>
            return Disp_Type (Node, "protected type");
         when Iir_Kind_Protected_Type_Body =>
            return Disp_Type (Node, "protected type body");
         when Iir_Kind_Subtype_Definition =>
            return "subtype definition";

         when Iir_Kind_Choice_By_Expression =>
            return "choice by expression";
         when Iir_Kind_Choice_By_Range =>
            return "choice by range";
         when Iir_Kind_Choice_By_Name =>
            return "choice by name";
         when Iir_Kind_Choice_By_Others =>
            return "others choice";
         when Iir_Kind_Choice_By_None =>
            return "positionnal choice";

         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Enumeration_Type_Definition =>
            return Iirs_Utils.Image_Identifier (Get_Type_Declarator (Node));
         when Iir_Kind_Function_Call =>
            return "function call";
         when Iir_Kind_Procedure_Call_Statement =>
            return "procedure call statement";
         when Iir_Kind_Procedure_Call =>
            return "procedure call";
         when Iir_Kind_Selected_Name =>
            Name_Table.Image (Get_Suffix_Identifier (Node));
            return '''
              & Name_Table.Name_Buffer (1 .. Name_Table.Name_Length)
              & ''';
         when Iir_Kind_Simple_Name =>
            Name_Table.Image (Get_Identifier (Node));
            return '''
              & Name_Table.Name_Buffer (1 .. Name_Table.Name_Length)
              & ''';
         when Iir_Kind_Entity_Aspect_Entity =>
            return "aspect " & Disp_Node (Get_Entity (Node))
              & '(' & Iirs_Utils.Image_Identifier (Get_Architecture (Node))
              & ')';
         when Iir_Kind_Entity_Aspect_Configuration =>
            return "configuration entity aspect";
         when Iir_Kind_Entity_Aspect_Open =>
            return "open entity aspect";

         when Iir_Kinds_Monadic_Operator
           | Iir_Kinds_Dyadic_Operator =>
            return "operator """
              & Name_Table.Image (Iirs_Utils.Get_Operator_Name (Node))
              & """";
         when Iir_Kind_Qualified_Expression =>
            return "qualified expression";
         when Iir_Kind_Type_Conversion =>
            return "type conversion";
         when Iir_Kind_Allocator_By_Subtype
           | Iir_Kind_Allocator_By_Expression =>
            return "allocator";
         when Iir_Kind_Indexed_Name =>
            return "indexed name";
         when Iir_Kind_Range_Expression =>
            return "range expression";
         when Iir_Kind_Implicit_Dereference =>
            return "implicit access dereference";
         when Iir_Kind_Dereference =>
            return "access dereference";
         when Iir_Kind_Selected_Element =>
            return "selected element";
         when Iir_Kind_Selected_By_All_Name =>
            return ".all name";
         when Iir_Kind_Psl_Expression =>
            return "PSL instantiation";

         when Iir_Kind_Constant_Interface_Declaration =>
            case Get_Kind (Get_Parent (Node)) is
               when Iir_Kind_Entity_Declaration
                 | Iir_Kind_Block_Statement
                 | Iir_Kind_Block_Header =>
                  return Disp_Identifier (Node, "generic");
               when others =>
                  return Disp_Identifier (Node, "constant interface");
            end case;
         when Iir_Kind_Signal_Interface_Declaration =>
            case Get_Kind (Get_Parent (Node)) is
               when Iir_Kind_Entity_Declaration
                 | Iir_Kind_Block_Statement
                 | Iir_Kind_Block_Header =>
                  return Disp_Identifier (Node, "port");
               when others =>
                  return Disp_Identifier (Node, "signal interface");
            end case;
         when Iir_Kind_Variable_Interface_Declaration =>
            return Disp_Identifier (Node, "variable interface");
         when Iir_Kind_File_Interface_Declaration =>
            return Disp_Identifier (Node, "file interface");
         when Iir_Kind_Signal_Declaration =>
            return Disp_Identifier (Node, "signal");
         when Iir_Kind_Variable_Declaration =>
            return Disp_Identifier (Node, "variable");
         when Iir_Kind_Iterator_Declaration
           | Iir_Kind_Constant_Declaration =>
            return Disp_Identifier (Node, "constant");
         when Iir_Kind_File_Declaration =>
            return Disp_Identifier (Node, "file");
         when Iir_Kind_Object_Alias_Declaration =>
            return Disp_Identifier (Node, "alias");
         when Iir_Kind_Non_Object_Alias_Declaration =>
            return Disp_Identifier (Node, "non-object alias");
         when Iir_Kind_Guard_Signal_Declaration =>
            return "GUARD signal";
         when Iir_Kind_Group_Template_Declaration =>
            return Disp_Identifier (Node, "group template");
         when Iir_Kind_Group_Declaration =>
            return Disp_Identifier (Node, "group");

         when Iir_Kind_Library_Declaration
           | Iir_Kind_Library_Clause =>
            return Disp_Identifier (Node, "library");
         when Iir_Kind_Design_File =>
            return "design file";

         when Iir_Kind_Procedure_Declaration =>
            return Disp_Identifier (Node, "procedure");
         when Iir_Kind_Procedure_Body
           | Iir_Kind_Function_Body =>
            return "subprogram body";
         when Iir_Kind_Function_Declaration =>
            return Disp_Identifier (Node, "function");

         when Iir_Kind_Package_Declaration =>
            return Disp_Identifier (Node, "package");
         when Iir_Kind_Package_Body =>
            return Disp_Identifier (Node, "package body");
         when Iir_Kind_Entity_Declaration =>
            return Disp_Identifier (Node, "entity");
         when Iir_Kind_Architecture_Declaration =>
            return Disp_Identifier (Node, "architecture") &
              " of" & Disp_Identifier (Get_Entity (Node), "");
         when Iir_Kind_Configuration_Declaration =>
            declare
               Id : Name_Id;
               Ent : Iir;
               Arch : Iir;
            begin
               Id := Get_Identifier (Node);
               if Id /= Null_Identifier then
                  return Disp_Identifier (Node, "configuration");
               else
                  Ent := Get_Library_Unit (Get_Entity (Node));
                  Arch := Get_Block_Specification
                    (Get_Block_Configuration (Node));
                  return "default configuration of "
                    & Iirs_Utils.Image_Identifier (Ent)
                    & '(' & Iirs_Utils.Image_Identifier (Arch) & ')';
               end if;
            end;
         when Iir_Kind_Component_Declaration =>
            return Disp_Identifier (Node, "component");

         when Iir_Kind_Design_Unit =>
            return Disp_Node (Get_Library_Unit (Node));
         when Iir_Kind_Use_Clause =>
            return "use clause";
         when Iir_Kind_Disconnection_Specification =>
            return "disconnection specification";

         when Iir_Kind_Slice_Name =>
            return "slice";
         when Iir_Kind_Parenthesis_Name =>
            return "function call, slice or indexed name";
         when Iir_Kind_Type_Declaration =>
            return Disp_Identifier (Node, "type");
         when Iir_Kind_Anonymous_Type_Declaration =>
            return Disp_Identifier (Node, "type");
         when Iir_Kind_Subtype_Declaration =>
            return Disp_Identifier (Node, "subtype");

         when Iir_Kind_Component_Instantiation_Statement =>
            return Disp_Identifier (Node, "component instance");
         when Iir_Kind_Configuration_Specification =>
            return "configuration specification";
         when Iir_Kind_Component_Configuration =>
            return "component configuration";
         when Iir_Kind_Implicit_Function_Declaration =>
            return Disp_Identifier (Node, "implicit function")
              & Disp_Identifier (Get_Type_Reference (Node), " of type");
--             return "implicit function "
--               & Iirs_Utils.Get_Predefined_Function_Name
--               (Get_Implicit_Definition (Node));
         when Iir_Kind_Implicit_Procedure_Declaration =>
            return "implicit procedure "
              & Iirs_Utils.Get_Predefined_Function_Name
              (Get_Implicit_Definition (Node));

         when Iir_Kind_Concurrent_Procedure_Call_Statement =>
            return "concurrent procedure call";
         when Iir_Kind_Generate_Statement =>
            return "generate statement";

         when Iir_Kind_Psl_Declaration =>
            return Disp_Identifier (Node, "PSL declaration");

         when Iir_Kind_Attribute_Declaration =>
            return Disp_Identifier (Node, "attribute");
         when Iir_Kind_Attribute_Specification =>
            return "attribute specification";
         when Iir_Kind_Entity_Class =>
            return "entity class";
         when Iir_Kind_Attribute_Value =>
            return "attribute value";
         when Iir_Kind_Attribute_Name =>
            return "attribute";
         when Iir_Kind_Base_Attribute =>
            return "'base attribute";
         when Iir_Kind_Length_Array_Attribute =>
            return "'length attribute";
         when Iir_Kind_Range_Array_Attribute =>
            return "'range attribute";
         when Iir_Kind_Reverse_Range_Array_Attribute =>
            return "'reverse_range attribute";
         when Iir_Kind_Ascending_Type_Attribute
           | Iir_Kind_Ascending_Array_Attribute =>
            return "'ascending attribute";
         when Iir_Kind_Left_Type_Attribute
           | Iir_Kind_Left_Array_Attribute =>
            return "'left attribute";
         when Iir_Kind_Right_Type_Attribute
           | Iir_Kind_Right_Array_Attribute =>
            return "'right attribute";
         when Iir_Kind_Low_Type_Attribute
           | Iir_Kind_Low_Array_Attribute =>
            return "'low attribute";
         when Iir_Kind_Leftof_Attribute =>
            return "'leftof attribute";
         when Iir_Kind_Rightof_Attribute =>
            return "'rightof attribute";
         when Iir_Kind_Pred_Attribute =>
            return "'pred attribute";
         when Iir_Kind_Succ_Attribute =>
            return "'succ attribute";
         when Iir_Kind_Pos_Attribute =>
            return "'pos attribute";
         when Iir_Kind_Val_Attribute =>
            return "'val attribute";
         when Iir_Kind_Image_Attribute =>
            return "'image attribute";
         when Iir_Kind_Value_Attribute =>
            return "'value attribute";
         when Iir_Kind_High_Type_Attribute
           | Iir_Kind_High_Array_Attribute =>
            return "'high attribute";
         when Iir_Kind_Transaction_Attribute =>
            return "'transaction attribute";
         when Iir_Kind_Stable_Attribute =>
            return "'stable attribute";
         when Iir_Kind_Quiet_Attribute =>
            return "'quiet attribute";
         when Iir_Kind_Delayed_Attribute =>
            return "'delayed attribute";
         when Iir_Kind_Driving_Attribute =>
            return "'driving attribute";
         when Iir_Kind_Driving_Value_Attribute =>
            return "'driving_value attribute";
         when Iir_Kind_Event_Attribute =>
            return "'event attribute";
         when Iir_Kind_Active_Attribute =>
            return "'active attribute";
         when Iir_Kind_Last_Event_Attribute =>
            return "'last_event attribute";
         when Iir_Kind_Last_Active_Attribute =>
            return "'last_active attribute";
         when Iir_Kind_Last_Value_Attribute =>
            return "'last_value attribute";
         when Iir_Kind_Behavior_Attribute =>
            return "'behavior attribute";
         when Iir_Kind_Structure_Attribute =>
            return "'structure attribute";

         when Iir_Kind_Path_Name_Attribute =>
            return "'path_name attribute";
         when Iir_Kind_Instance_Name_Attribute =>
            return "'instance_name attribute";
         when Iir_Kind_Simple_Name_Attribute =>
            return "'simple_name attribute";

         when Iir_Kind_For_Loop_Statement =>
            return Disp_Label (Node, "for loop statement");
         when Iir_Kind_While_Loop_Statement =>
            return Disp_Label (Node, "loop statement");
         when Iir_Kind_Process_Statement
           | Iir_Kind_Sensitized_Process_Statement =>
            return Disp_Label (Node, "process");
         when Iir_Kind_Block_Statement =>
            return Disp_Label (Node, "block statement");
         when Iir_Kind_Block_Header =>
            return "block header";
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
            return Disp_Label
              (Node, "concurrent conditional signal assignment");
         when Iir_Kind_Concurrent_Selected_Signal_Assignment =>
            return Disp_Label
              (Node, "concurrent selected signal assignment");
         when Iir_Kind_Concurrent_Assertion_Statement =>
            return Disp_Label (Node, "concurrent assertion");
         when Iir_Kind_Psl_Assert_Statement =>
            return Disp_Label (Node, "PSL assertion");
         when Iir_Kind_Psl_Default_Clock =>
            return "PSL default clock";

         when Iir_Kind_If_Statement =>
            return Disp_Label (Node, "if statement");
         when Iir_Kind_Elsif =>
            return Disp_Label (Node, "else/elsif statement");
         when Iir_Kind_Next_Statement =>
            return Disp_Label (Node, "next statement");
         when Iir_Kind_Exit_Statement =>
            return Disp_Label (Node, "exit statement");
         when Iir_Kind_Case_Statement =>
            return Disp_Label (Node, "case statement");
         when Iir_Kind_Return_Statement =>
            return Disp_Label (Node, "return statement");
         when Iir_Kind_Signal_Assignment_Statement =>
            return Disp_Label (Node, "signal assignment statement");
         when Iir_Kind_Variable_Assignment_Statement =>
            return Disp_Label (Node, "variable assignment statement");
         when Iir_Kind_Null_Statement =>
            return Disp_Label (Node, "null statement");
         when Iir_Kind_Wait_Statement =>
            return Disp_Label (Node, "wait statement");
         when Iir_Kind_Assertion_Statement =>
            return Disp_Label (Node, "assertion statement");
         when Iir_Kind_Report_Statement =>
            return Disp_Label (Node, "report statement");

         when Iir_Kind_Block_Configuration =>
            return "block configuration";
         when Iir_Kind_Binding_Indication =>
            return "binding indication";


         when Iir_Kind_Error =>
            return "error";

--           when others =>
--              Error_Kind ("disp_node", Node);
--              return "???";
      end case;
   end Disp_Node;

   -- Disp a node location.
   -- Used for output of message.

   function Get_Location_Str
     (Name : Name_Id; Line, Col : Natural; Filename : Boolean)
     return String
   is
      Line_Str : constant String := Natural'Image (Line);
      Col_Str : constant String := Natural'Image (Col);
   begin
      if Filename then
         return Name_Table.Image (Name)
           & ':' & Line_Str (Line_Str'First + 1 .. Line_Str'Last)
           & ':' & Col_Str (Col_Str'First + 1 .. Col_Str'Last);
      else
         return Line_Str (Line_Str'First + 1 .. Line_Str'Last)
           & ':' & Col_Str (Col_Str'First + 1 .. Col_Str'Last);
      end if;
   end Get_Location_Str;

   function Get_Location_Str (Loc : Location_Type; Filename : Boolean := True)
     return string
   is
      Line, Col : Natural;
      Name : Name_Id;
   begin
      if Loc = Location_Nil then
         --  Avoid a crash.
         return "??:??:??:";
      else
         Location_To_Position (Loc, Name, Line, Col);
         return Get_Location_Str (Name, Line, Col, Filename);
      end if;
   end Get_Location_Str;

   function Disp_Location (Node: Iir) return String is
   begin
      return Get_Location_Str (Get_Location (Node));
   end Disp_Location;

   function Disp_Name (Kind : Iir_Kind) return String is
   begin
      case Kind is
         when Iir_Kind_Constant_Declaration =>
            return "constant declaration";
         when Iir_Kind_Signal_Declaration =>
            return "signal declaration";
         when Iir_Kind_Variable_Declaration =>
            return "variable declaration";
         when Iir_Kind_File_Declaration =>
            return "file declaration";
         when others =>
            return "???" & Iir_Kind'Image (Kind);
      end case;
   end Disp_Name;

   function Image (N : Iir_Int64) return String
   is
      Res : constant String := Iir_Int64'Image (N);
   begin
      if Res (1) = ' ' then
         return Res (2 .. Res'Last);
      else
         return Res;
      end if;
   end Image;

   function Disp_Discrete (Dtype : Iir; Pos : Iir_Int64) return String is
   begin
      case Get_Kind (Dtype) is
         when Iir_Kind_Integer_Type_Definition =>
            return Image (Pos);
         when Iir_Kind_Enumeration_Type_Definition =>
            return Name_Table.Image
              (Get_Identifier (Get_Nth_Element
                               (Get_Enumeration_Literal_List (Dtype),
                                Natural (Pos))));
         when others =>
            Error_Kind ("disp_discrete", Dtype);
      end case;
   end Disp_Discrete;

   function Disp_Subprg (Subprg : Iir) return String
   is
      use Ada.Strings.Unbounded;
      Res : Unbounded_String;

      procedure Append_Type (Def : Iir)
      is
         use Name_Table;
         Decl : Iir := Get_Type_Declarator (Def);
      begin
         if Decl = Null_Iir then
            Decl := Get_Type_Declarator (Get_Base_Type (Def));
         end if;
         Image (Get_Identifier (Decl));
         Append (Res, Name_Buffer (1 .. Name_Length));
      end Append_Type;

   begin
      case Get_Kind (Subprg) is
         when Iir_Kind_Enumeration_Literal =>
            Append (Res, "enumeration literal ");
         when Iir_Kind_Implicit_Function_Declaration =>
            Append (Res, "implicit function ");
         when Iir_Kind_Implicit_Procedure_Declaration =>
            Append (Res, "implicit procedure ");
         when Iir_Kind_Function_Declaration =>
            Append (Res, "function ");
         when Iir_Kind_Procedure_Declaration =>
            Append (Res, "procedure ");
         when others =>
            Error_Kind ("disp_subprg", Subprg);
      end case;

      declare
         use Name_Table;

         Id : constant Name_Id := Get_Identifier (Subprg);
      begin
         Image (Id);
         case Id is
            when Std_Names.Name_Id_Operators
              | Std_Names.Name_Word_Operators
              | Std_Names.Name_Xnor
              | Std_Names.Name_Shift_Operators =>
               Append (Res, """");
               Append (Res, Name_Buffer (1 .. Name_Length));
               Append (Res, """");
            when others =>
               Append (Res, Name_Buffer (1 .. Name_Length));
         end case;
      end;

      Append (Res, " [");

      case Get_Kind (Subprg) is
         when Iir_Kind_Implicit_Function_Declaration
           | Iir_Kind_Implicit_Procedure_Declaration
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration =>
            declare
               El : Iir;
            begin
               El := Get_Interface_Declaration_Chain (Subprg);
               while El /= Null_Iir loop
                  Append_Type (Get_Type (El));
                  El := Get_Chain (El);
                  exit when El = Null_Iir;
                  Append (Res, ", ");
               end loop;
            end;
         when others =>
            null;
      end case;

      case Get_Kind (Subprg) is
         when Iir_Kind_Implicit_Function_Declaration
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Enumeration_Literal =>
            Append (Res, " return ");
            Append_Type (Get_Return_Type (Subprg));
         when others =>
            null;
      end case;

      Append (Res, "]");

      return To_String (Res);
   end Disp_Subprg;

   --  DEF must be any type definition.
   --  Return the type name of DEF, handle anonymous subtypes.
   function Disp_Type_Name (Def : Iir) return String
   is
      use Iirs_Utils;
      Decl : Iir;
   begin
      Decl := Get_Type_Declarator (Def);
      if Decl /= Null_Iir then
         return Image_Identifier (Decl);
      end if;
      Decl := Get_Type_Declarator (Get_Base_Type (Def));
      if Decl /= Null_Iir then
         return "a subtype of " & Image_Identifier (Decl);
      else
         return "an unknown type";
      end if;
   end Disp_Type_Name;

   function Disp_Type_Of (Node : Iir) return String
   is
      A_Type : Iir;
   begin
      A_Type := Get_Type (Node);
      if A_Type = Null_Iir then
         return "unknown";
      elsif Get_Kind (A_Type) = Iir_Kind_Overload_List then
         declare
            use Ada.Strings.Unbounded;
            Res : Unbounded_String;
            List : Iir_List;
            El : Iir;
            Nbr : Natural;
         begin
            List := Get_Overload_List (A_Type);
            Nbr := Get_Nbr_Elements (List);
            if Nbr = 0 then
               return "unknown";
            elsif Nbr = 1 then
               return Disp_Type_Name (Get_First_Element (List));
            else
               Append (Res, "one of ");
               for I in 0 .. Nbr - 1 loop
                  El := Get_Nth_Element (List, I);
                  Append (Res, Disp_Type_Name (El));
                  if I < Nbr - 2 then
                     Append (Res, ", ");
                  elsif I = Nbr - 2 then
                     Append (Res, " or ");
                  end if;
               end loop;
               return To_String (Res);
            end if;
         end;
      else
         return Disp_Type_Name (A_Type);
      end if;
   end Disp_Type_Of;

   procedure Error_Pure (Caller : Iir; Callee : Iir; Loc : Iir)
   is
      L : Location_Type;
   begin
      if Loc = Null_Iir then
         L := Get_Location (Caller);
      else
         L := Get_Location (Loc);
      end if;
      Error_Msg_Sem
        ("pure " & Disp_Node (Caller) & " cannot call (impure) "
         & Disp_Node (Callee), L);
      Error_Msg_Sem
        ("(" & Disp_Node (Callee) & " is defined here)", Callee);
   end Error_Pure;

   procedure Error_Not_Match (Expr: Iir; A_Type: Iir; Loc : Iir)
   is
   begin
      Error_Msg_Sem ("can't match " & Disp_Node (Expr) & " with type "
                     & Disp_Node (A_Type), Loc);
      if Loc /= Expr then
         Error_Msg_Sem ("(location of " & Disp_Node (Expr) & ")", Expr);
      end if;
   end Error_Not_Match;

end Errorout;
