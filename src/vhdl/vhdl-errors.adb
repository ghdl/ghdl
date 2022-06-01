--  Error message handling for vhdl.
--  Copyright (C) 2002-2019 Tristan Gingold
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

with Flags; use Flags;
with Name_Table;
with Files_Map;
with Vhdl.Utils; use Vhdl.Utils;
with Ada.Strings.Unbounded;
with Std_Names;
with Logging; use Logging;

package body Vhdl.Errors is
   procedure Error_Kind (Msg : String; N : Iir) is
   begin
      Log_Line (Msg & ": cannot handle " & Iir_Kind'Image (Get_Kind (N))
                  & " (" & Disp_Location (N) & ')');
      raise Internal_Error;
   end Error_Kind;

   procedure Error_Kind (Msg : String; Def : Iir_Predefined_Functions) is
   begin
      Log_Line
        (Msg & ": cannot handle " & Iir_Predefined_Functions'Image (Def));
      raise Internal_Error;
   end Error_Kind;

   function Get_Location_Safe (N : Iir) return Location_Type is
   begin
      if N = Null_Iir then
         return Location_Nil;
      else
         return Get_Location (N);
      end if;
   end Get_Location_Safe;

   function "+" (L : Iir) return Location_Type renames Get_Location_Safe;

   function "+" (L : Iir) return Source_Coord_Type is
   begin
      return +Get_Location_Safe (L);
   end "+";

   procedure Warning_Msg_Sem (Id : Msgid_Warnings;
                              Loc : Location_Type;
                              Msg: String;
                              Args : Earg_Arr := No_Eargs) is
   begin
      if Flags.Flag_Only_Elab_Warnings then
         return;
      end if;
      Report_Msg (Id, Semantic, +Loc, Msg, Args);
   end Warning_Msg_Sem;

   procedure Warning_Msg_Sem (Id : Msgid_Warnings;
                              Loc : Location_Type;
                              Msg: String;
                              Arg1 : Earg_Type) is
   begin
      Warning_Msg_Sem (Id, Loc, Msg, Earg_Arr'(1 => Arg1));
   end Warning_Msg_Sem;

   procedure Warning_Msg_Elab (Id : Msgid_Warnings;
                               Loc : Iir;
                               Msg: String;
                               Arg1 : Earg_Type) is
   begin
      Report_Msg (Id, Elaboration, +Loc, Msg, Earg_Arr'(1 => Arg1));
   end Warning_Msg_Elab;

   procedure Warning_Msg_Elab (Id : Msgid_Warnings;
                               Loc : Iir;
                               Msg: String;
                               Args : Earg_Arr := No_Eargs) is
   begin
      Report_Msg (Id, Elaboration, +Loc, Msg, Args);
   end Warning_Msg_Elab;

   procedure Error_Msg_Sem (Loc: Location_Type;
                            Msg: String;
                            Args : Earg_Arr := No_Eargs) is
   begin
      Report_Msg (Msgid_Error, Semantic, +Loc, Msg, Args);
   end Error_Msg_Sem;

   procedure Error_Msg_Sem
     (Loc: Location_Type; Msg: String; Arg1 : Earg_Type) is
   begin
      Report_Msg (Msgid_Error, Semantic, +Loc, Msg, (1 => Arg1));
   end Error_Msg_Sem;

   Relaxed_Hint_Done : Boolean := False;

   procedure Error_Msg_Relaxed (Origin : Report_Origin;
                                Id : Msgid_Warnings;
                                Msg : String;
                                Loc : Iir;
                                Args : Earg_Arr := No_Eargs)
   is
      Level : Msgid_Type;
   begin
      if Flag_Relaxed_Rules then
         if not Is_Warning_Enabled (Id) then
            return;
         end if;
         Level := Id;
      else
         Level := Msgid_Error;
      end if;
      Report_Msg (Level, Origin, +Loc, Msg, Args);
      if not Relaxed_Hint_Done and then Level = Msgid_Error then
         Report_Msg
           (Msgid_Note, Origin, +Loc,
            "(you can use -frelaxed to turn this error into a warning)");
         --  Emit the message only once, although it applies for many error.
         --  Maybe do it once per Id ?
         Relaxed_Hint_Done := True;
      end if;
   end Error_Msg_Relaxed;

   procedure Error_Msg_Sem_Relaxed (Loc : Iir;
                                    Id : Msgid_Warnings;
                                    Msg : String;
                                    Args : Earg_Arr := No_Eargs) is
   begin
      Error_Msg_Relaxed (Semantic, Id, Msg, Loc, Args);
   end Error_Msg_Sem_Relaxed;

   -- Disp a message during elaboration.
   procedure Error_Msg_Elab
     (Msg: String; Args : Earg_Arr := No_Eargs) is
   begin
      Report_Msg (Msgid_Error, Elaboration, No_Source_Coord, Msg, Args);
   end Error_Msg_Elab;

   procedure Error_Msg_Elab
     (Msg: String; Arg1 : Earg_Type) is
   begin
      Error_Msg_Elab (Msg, Earg_Arr'(1 => Arg1));
   end Error_Msg_Elab;

   procedure Error_Msg_Elab
     (Loc: Iir; Msg: String; Args : Earg_Arr := No_Eargs) is
   begin
      Report_Msg (Msgid_Error, Elaboration, +Loc, Msg, Args);
   end Error_Msg_Elab;

   procedure Error_Msg_Elab
     (Loc: Iir; Msg: String; Arg1 : Earg_Type) is
   begin
      Error_Msg_Elab (Loc, Msg, Earg_Arr'(1 => Arg1));
   end Error_Msg_Elab;

   procedure Error_Msg_Elab_Relaxed (Loc : Iir;
                                     Id : Msgid_Warnings;
                                     Msg : String;
                                     Args : Earg_Arr := No_Eargs) is
   begin
      Error_Msg_Relaxed (Elaboration, Id, Msg, Loc, Args);
   end Error_Msg_Elab_Relaxed;

   -- Disp a bug message.
   procedure Error_Internal (Expr: in Iir; Msg: String := "")
   is
      pragma Unreferenced (Expr);
   begin
      Log ("internal error: ");
      Log_Line (Msg);
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
            return "anonymous " & Str
              & " defined at " & Disp_Location (Node);
         else
            return Disp_Identifier (Decl, Str);
         end if;
      end Disp_Type;

      function Disp_Nature (Node : Iir; Str : String) return String
      is
         Decl: Iir;
      begin
         Decl := Get_Nature_Declarator (Node);
         if Decl = Null_Iir then
            return "anonymous " & Str
              & " defined at " & Disp_Location (Node);
         else
            return Disp_Identifier (Decl, Str);
         end if;
      end Disp_Nature;

   begin
      case Get_Kind (Node) is
         when Iir_Kind_String_Literal8 =>
            return "string literal";
         when Iir_Kind_Character_Literal =>
            return "character literal " & Image_Identifier (Node);
         when Iir_Kind_Integer_Literal =>
            return "integer literal";
         when Iir_Kind_Floating_Point_Literal =>
            return "floating point literal";
         when Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal =>
            return "physical literal";
         when Iir_Kind_Enumeration_Literal =>
            return "enumeration literal " & Image_Identifier (Node);
         when Iir_Kind_Element_Declaration
           | Iir_Kind_Nature_Element_Declaration =>
            return Disp_Identifier (Node, "element");
         when Iir_Kind_Record_Element_Constraint =>
            return "record element constraint " & Image_Identifier (Node);
         when Iir_Kind_Array_Element_Resolution =>
            return "array element resolution";
         when Iir_Kind_Record_Resolution =>
            return "record resolution";
         when Iir_Kind_Record_Element_Resolution =>
            return "record element resolution";
         when Iir_Kind_Null_Literal =>
            return "null literal";
         when Iir_Kind_Overflow_Literal =>
            return Disp_Node (Get_Literal_Origin (Node));
         when Iir_Kind_Unaffected_Waveform =>
            return "unaffected waveform";
         when Iir_Kind_Aggregate =>
            return "aggregate";
         when Iir_Kind_Unit_Declaration =>
            return Disp_Identifier (Node, "physical unit");
         when Iir_Kind_Simple_Aggregate =>
            return "locally static array literal";

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
         when Iir_Kind_Conditional_Expression =>
            return "conditional expression";
         when Iir_Kind_Association_Element_Open =>
            return "open association element";
         when Iir_Kind_Association_Element_By_Individual =>
            return "individual association element";
         when Iir_Kind_Association_Element_By_Expression
           | Iir_Kind_Association_Element_By_Name
           | Iir_Kind_Association_Element_Package
           | Iir_Kind_Association_Element_Type
           | Iir_Kind_Association_Element_Subprogram
           | Iir_Kind_Association_Element_Terminal =>
            return "association element";
         when Iir_Kind_Overload_List =>
            return "overloaded name or expression";

         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Enumeration_Type_Definition =>
            return Image_Identifier (Get_Type_Declarator (Node));
         when Iir_Kind_Wildcard_Type_Definition =>
            return "<any>";
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
         when Iir_Kind_Interface_Type_Definition =>
            return Disp_Type (Node, "interface type");
         when Iir_Kind_Protected_Type_Declaration =>
            return Disp_Type (Node, "protected type");
         when Iir_Kind_Protected_Type_Body =>
            return Disp_Type (Node, "protected type body");
         when Iir_Kind_Subtype_Definition =>
            return "subtype definition";
         when Iir_Kind_Foreign_Vector_Type_Definition =>
            return "foreign vector type definition";

         when Iir_Kind_Scalar_Nature_Definition =>
            return Disp_Nature (Node, "scalar nature");
         when Iir_Kind_Array_Nature_Definition =>
            return Disp_Nature (Node, "array nature");
         when Iir_Kind_Array_Subnature_Definition =>
            return Disp_Nature (Node, "array subnature");
         when Iir_Kind_Record_Nature_Definition =>
            return Disp_Nature (Node, "record nature");

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

         when Iir_Kind_Function_Call =>
            return "function call";
         when Iir_Kind_Procedure_Call_Statement =>
            return "procedure call statement";
         when Iir_Kind_Procedure_Call =>
            return "procedure call";
         when Iir_Kind_Selected_Name =>
            return ''' & Name_Table.Image (Get_Identifier (Node)) & ''';
         when Iir_Kind_Simple_Name =>
            return ''' & Name_Table.Image (Get_Identifier (Node)) & ''';
         when Iir_Kind_Reference_Name =>
            --  Shouldn't happen.
            return "name";
         when Iir_Kind_External_Constant_Name =>
            return "external constant name";
         when Iir_Kind_External_Signal_Name =>
            return "external signal name";
         when Iir_Kind_External_Variable_Name =>
            return "external variable name";

         when Iir_Kind_Package_Pathname =>
            return "package pathname";
         when Iir_Kind_Absolute_Pathname =>
            return "absolute pathname";
         when Iir_Kind_Relative_Pathname =>
            return "relative pathname";
         when Iir_Kind_Pathname_Element =>
            return "pathname element";

         when Iir_Kind_Entity_Aspect_Entity =>
            declare
               Arch : constant Iir := Get_Architecture (Node);
               Ent : constant Iir := Get_Entity (Node);
            begin
               if Arch = Null_Iir then
                  return "aspect " & Disp_Node (Ent);
               else
                  return "aspect " & Disp_Node (Ent)
                    & '(' & Image_Identifier (Arch) & ')';
               end if;
            end;
         when Iir_Kind_Entity_Aspect_Configuration =>
            return "configuration entity aspect";
         when Iir_Kind_Entity_Aspect_Open =>
            return "open entity aspect";
         when Iir_Kind_Psl_Hierarchical_Name =>
            return "hierarchical name";

         when Iir_Kinds_Monadic_Operator
           | Iir_Kinds_Dyadic_Operator =>
            return "operator """
              & Name_Table.Image (Get_Operator_Name (Node)) & """";
         when Iir_Kind_Parenthesis_Expression =>
            return "expression";
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
         when Iir_Kind_Break_Element =>
            return "break element";

         when Iir_Kind_Interface_Constant_Declaration =>
            if Get_Parent (Node) = Null_Iir then
               --  For constant interface of predefined operator.
               return "anonymous interface";
            end if;
            case Get_Kind (Get_Parent (Node)) is
               when Iir_Kind_Entity_Declaration
                 | Iir_Kind_Block_Statement
                 | Iir_Kind_Block_Header =>
                  return Disp_Identifier (Node, "generic");
               when others =>
                  return Disp_Identifier (Node, "constant interface");
            end case;
         when Iir_Kind_Interface_Signal_Declaration =>
            case Get_Kind (Get_Parent (Node)) is
               when Iir_Kind_Entity_Declaration
                 | Iir_Kind_Block_Statement
                 | Iir_Kind_Block_Header =>
                  return Disp_Identifier (Node, "port");
               when others =>
                  return Disp_Identifier (Node, "signal interface");
            end case;
         when Iir_Kind_Interface_Variable_Declaration =>
            return Disp_Identifier (Node, "variable interface");
         when Iir_Kind_Interface_File_Declaration =>
            return Disp_Identifier (Node, "file interface");
         when Iir_Kind_Interface_Quantity_Declaration =>
            return Disp_Identifier (Node, "quantity interface");
         when Iir_Kind_Interface_Terminal_Declaration =>
            return Disp_Identifier (Node, "terminal interface");
         when Iir_Kind_Interface_Package_Declaration =>
            return Disp_Identifier (Node, "package interface");
         when Iir_Kind_Interface_Type_Declaration =>
            return Disp_Identifier (Node, "type interface");
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
         when Iir_Kind_Signal_Attribute_Declaration =>
            --  Should not appear.
            return "signal attribute";
         when Iir_Kind_Suspend_State_Declaration =>
            --  Should not appear.
            return "suspend state variable";
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
         when Iir_Kind_Function_Declaration =>
            return Disp_Identifier (Node, "function");
         when Iir_Kind_Function_Instantiation_Declaration =>
            return Disp_Identifier (Node, "function instantiation");
         when Iir_Kind_Procedure_Instantiation_Declaration =>
            return Disp_Identifier (Node, "procedure instantiation");
         when Iir_Kind_Interface_Procedure_Declaration =>
            return Disp_Identifier (Node, "interface procedure");
         when Iir_Kind_Interface_Function_Declaration =>
            return Disp_Identifier (Node, "interface function");
         when Iir_Kind_Procedure_Body
           | Iir_Kind_Function_Body =>
            return "subprogram body";

         when Iir_Kind_Foreign_Module =>
            return Disp_Identifier (Node, "foreign module");

         when Iir_Kind_Package_Declaration =>
            return Disp_Identifier (Node, "package");
         when Iir_Kind_Package_Body =>
            return Disp_Identifier (Node, "package body");
         when Iir_Kind_Entity_Declaration =>
            return Disp_Identifier (Node, "entity");
         when Iir_Kind_Architecture_Body =>
            return Disp_Identifier (Node, "architecture") &
              " of" & Disp_Identifier (Get_Entity_Name (Node), "");
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
                  Ent := Get_Entity (Node);
                  Arch := Get_Block_Specification
                    (Get_Block_Configuration (Node));
                  return "default configuration of "
                    & Image_Identifier (Ent)
                    & '(' & Image_Identifier (Arch) & ')';
               end if;
            end;
         when Iir_Kind_Context_Declaration =>
            return Disp_Identifier (Node, "context");
         when Iir_Kind_Package_Instantiation_Declaration =>
            return Disp_Identifier (Node, "instantiation package");
         when Iir_Kind_Vmode_Declaration =>
            return Disp_Identifier (Node, "vmode");
         when Iir_Kind_Vprop_Declaration =>
            return Disp_Identifier (Node, "vprop");
         when Iir_Kind_Vunit_Declaration =>
            return Disp_Identifier (Node, "vunit");

         when Iir_Kind_Package_Header =>
            return "package header";

         when Iir_Kind_Component_Declaration =>
            return Disp_Identifier (Node, "component");

         when Iir_Kind_Design_Unit =>
            return Disp_Node (Get_Library_Unit (Node));
         when Iir_Kind_Use_Clause =>
            return "use clause";
         when Iir_Kind_Context_Reference =>
            return "context reference";
         when Iir_Kind_PSL_Inherit_Spec =>
            return "PSL inherit";
         when Iir_Kind_Disconnection_Specification =>
            return "disconnection specification";
         when Iir_Kind_Step_Limit_Specification =>
            return "step limit specification";

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

         when Iir_Kind_Nature_Declaration =>
            return Disp_Identifier (Node, "nature");
         when Iir_Kind_Subnature_Declaration =>
            return Disp_Identifier (Node, "subnature");

         when Iir_Kind_Component_Instantiation_Statement =>
            return Disp_Identifier (Node, "component instance");
         when Iir_Kind_Configuration_Specification =>
            return "configuration specification";
         when Iir_Kind_Component_Configuration =>
            return "component configuration";

         when Iir_Kind_Concurrent_Procedure_Call_Statement =>
            return "concurrent procedure call";
         when Iir_Kind_For_Generate_Statement =>
            return "for generate statement";
         when Iir_Kind_If_Generate_Statement
           | Iir_Kind_If_Generate_Else_Clause =>
            return "if generate statement";
         when Iir_Kind_Case_Generate_Statement =>
            return "case generate statement";
         when Iir_Kind_Generate_Statement_Body =>
            return "generate statement body";

         when Iir_Kind_Simple_Simultaneous_Statement =>
            return "simple simultaneous statement";
         when Iir_Kind_Simultaneous_Null_Statement =>
            return "simultaneous null statement";
         when Iir_Kind_Simultaneous_Procedural_Statement =>
            return "simultaneous procedural statement";
         when Iir_Kind_Simultaneous_Case_Statement =>
            return "simultaneous case statement";
         when Iir_Kind_Simultaneous_If_Statement
           | Iir_Kind_Simultaneous_Elsif =>
            return "simultaneous if statement";

         when Iir_Kind_Psl_Declaration =>
            return Disp_Identifier (Node, "PSL declaration");
         when Iir_Kind_Psl_Endpoint_Declaration =>
            return Disp_Identifier (Node, "PSL endpoint declaration");

         when Iir_Kind_Terminal_Declaration =>
            return Disp_Identifier (Node, "terminal declaration");
         when Iir_Kind_Free_Quantity_Declaration
           | Iir_Kind_Across_Quantity_Declaration
           | Iir_Kind_Through_Quantity_Declaration
           | Iir_Kind_Spectrum_Quantity_Declaration
           | Iir_Kind_Noise_Quantity_Declaration =>
            return Disp_Identifier (Node, "quantity declaration");

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
         when Iir_Kind_Across_Attribute =>
            return "'across attribute";
         when Iir_Kind_Through_Attribute =>
            return "'through attribute";
         when Iir_Kind_Nature_Reference_Attribute =>
            return "'reference attribute";
         when Iir_Kind_Length_Array_Attribute =>
            return "'length attribute";
         when Iir_Kind_Range_Array_Attribute =>
            return "'range attribute";
         when Iir_Kind_Reverse_Range_Array_Attribute =>
            return "'reverse_range attribute";
         when Iir_Kind_Subtype_Attribute =>
            return "'subtype attribute";
         when Iir_Kind_Element_Attribute =>
            return "'element attribute";
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
         when Iir_Kind_Signal_Slew_Attribute
           | Iir_Kind_Quantity_Slew_Attribute =>
            return "'slew attribute";
         when Iir_Kind_Zoh_Attribute =>
            return "'zoh attribute";
         when Iir_Kind_Ltf_Attribute =>
            return "'ltf attribute";
         when Iir_Kind_Ztf_Attribute =>
            return "'ztf attribute";
         when Iir_Kind_Ramp_Attribute =>
            return "'ramp attribute";
         when Iir_Kind_Dot_Attribute =>
            return "'dot attribute";
         when Iir_Kind_Integ_Attribute =>
            return "'integ attribute";
         when Iir_Kind_Above_Attribute =>
            return "'above attribute";
         when Iir_Kind_Transaction_Attribute =>
            return "'transaction attribute";
         when Iir_Kind_Stable_Attribute =>
            return "'stable attribute";
         when Iir_Kind_Quiet_Attribute =>
            return "'quiet attribute";
         when Iir_Kind_Delayed_Attribute
           | Iir_Kind_Quantity_Delayed_Attribute =>
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
         when Iir_Kind_Concurrent_Simple_Signal_Assignment =>
            return Disp_Label
              (Node, "concurrent simple signal assignment");
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
            return Disp_Label
              (Node, "concurrent conditional signal assignment");
         when Iir_Kind_Concurrent_Selected_Signal_Assignment =>
            return Disp_Label
              (Node, "concurrent selected signal assignment");
         when Iir_Kind_Concurrent_Assertion_Statement =>
            return Disp_Label (Node, "concurrent assertion");
         when Iir_Kind_Concurrent_Break_Statement =>
            return Disp_Label (Node, "concurrent break statement");

         when Iir_Kind_Psl_Assert_Directive =>
            return Disp_Label (Node, "PSL assertion");
         when Iir_Kind_Psl_Assume_Directive =>
            return Disp_Label (Node, "PSL assumption");
         when Iir_Kind_Psl_Cover_Directive =>
            return Disp_Label (Node, "PSL cover");
         when Iir_Kind_Psl_Restrict_Directive =>
            return "PSL restrict";
         when Iir_Kind_Psl_Default_Clock =>
            return "PSL default clock";
         when Iir_Kind_Psl_Prev =>
            return "PSL prev function";
         when Iir_Kind_Psl_Stable =>
            return "PSL stable function";
         when Iir_Kind_Psl_Rose =>
            return "PSL rose function";
         when Iir_Kind_Psl_Fell =>
            return "PSL fell function";
         when Iir_Kind_Psl_Onehot =>
            return "PSL onehot function";
         when Iir_Kind_Psl_Onehot0 =>
            return "PSL onehot0 function";

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
         when Iir_Kind_Simple_Signal_Assignment_Statement =>
            return Disp_Label (Node, "signal assignment statement");
         when Iir_Kind_Conditional_Signal_Assignment_Statement =>
            return Disp_Label
              (Node, "conditional signal assignment statement");
         when Iir_Kind_Selected_Waveform_Assignment_Statement =>
            return Disp_Label
              (Node, "selected waveform assignment statement");
         when Iir_Kind_Signal_Force_Assignment_Statement =>
            return Disp_Label (Node, "signal force assignment");
         when Iir_Kind_Signal_Release_Assignment_Statement =>
            return Disp_Label (Node, "signal release assignment");
         when Iir_Kind_Variable_Assignment_Statement =>
            return Disp_Label (Node, "variable assignment statement");
         when Iir_Kind_Conditional_Variable_Assignment_Statement =>
            return Disp_Label
              (Node, "conditional variable assignment statement");
         when Iir_Kind_Null_Statement =>
            return Disp_Label (Node, "null statement");
         when Iir_Kind_Wait_Statement =>
            return Disp_Label (Node, "wait statement");
         when Iir_Kind_Assertion_Statement =>
            return Disp_Label (Node, "assertion statement");
         when Iir_Kind_Report_Statement =>
            return Disp_Label (Node, "report statement");
         when Iir_Kind_Break_Statement =>
            return Disp_Label (Node, "break statement");
         when Iir_Kind_Suspend_State_Statement =>
            --  Should not appear.
            return "suspend state statement";

         when Iir_Kind_Block_Configuration =>
            return "block configuration";
         when Iir_Kind_Binding_Indication =>
            return "binding indication";

         when Iir_Kind_Error =>
            return "error";
         when Iir_Kind_Unused =>
            return "*unused*";
      end case;
   end Disp_Node;

   -- Disp a node location.
   -- Used for output of message.

   function Disp_Location (Node: Iir) return String is
   begin
      return Files_Map.Image (Get_Location (Node));
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

   function Image (N : Int64) return String
   is
      Res : constant String := Int64'Image (N);
   begin
      if Res (1) = ' ' then
         return Res (2 .. Res'Last);
      else
         return Res;
      end if;
   end Image;

   function Disp_Discrete (Dtype : Iir; Pos : Int64) return String is
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

      --  Cf code in evaluation for 'instance_name ?
      procedure Append_Type (Def : Iir)
      is
         use Name_Table;
         Decl : Iir := Get_Type_Declarator (Def);
      begin
         if Decl = Null_Iir then
            Decl := Get_Type_Declarator (Get_Base_Type (Def));
            if Decl = Null_Iir then
               Append (Res, "*unknown*");
               return;
            end if;
         end if;
         Append (Res, Image (Get_Identifier (Decl)));
      end Append_Type;

   begin
      case Get_Kind (Subprg) is
         when Iir_Kind_Enumeration_Literal =>
            Append (Res, "enumeration literal ");
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Interface_Function_Declaration =>
            Append (Res, "function ");
         when Iir_Kind_Procedure_Declaration
           | Iir_Kind_Interface_Procedure_Declaration =>
            Append (Res, "procedure ");
         when others =>
            Error_Kind ("disp_subprg", Subprg);
      end case;

      declare
         use Name_Table;

         Id : constant Name_Id := Get_Identifier (Subprg);
      begin
         case Id is
            when Std_Names.Name_Id_Operators
              | Std_Names.Name_Word_Operators
              | Std_Names.Name_Xnor
              | Std_Names.Name_Shift_Operators =>
               Append (Res, """");
               Append (Res, Image (Id));
               Append (Res, """");
            when others =>
               Append (Res, Image (Id));
         end case;
      end;

      Append (Res, " [");

      case Get_Kind (Subprg) is
         when Iir_Kinds_Subprogram_Declaration
           | Iir_Kinds_Interface_Subprogram_Declaration =>
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
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Interface_Function_Declaration
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
      Decl : Iir;
   begin
      if Is_Error (Def) then
         return "an erroneous type";
      end if;
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
            List : constant Iir_List := Get_Overload_List (A_Type);
            Nbr : constant Natural := Get_Nbr_Elements (List);
            Res : Unbounded_String;
            El : Iir;
            It : List_Iterator;
         begin
            if Nbr = 0 then
               return "unknown";
            elsif Nbr = 1 then
               return Disp_Type_Name (Get_First_Element (List));
            else
               Append (Res, "one of ");
               It := List_Iterate (List);
               for I in 0 .. Nbr - 1 loop
                  pragma Assert (Is_Valid (It));
                  El := Get_Element (It);
                  Append (Res, Disp_Type_Name (El));
                  if I < Nbr - 2 then
                     Append (Res, ", ");
                  elsif I = Nbr - 2 then
                     Append (Res, " or ");
                  end if;
                  Next (It);
               end loop;
               return To_String (Res);
            end if;
         end;
      else
         return Disp_Type_Name (A_Type);
      end if;
   end Disp_Type_Of;

   procedure Error_Pure
     (Origin : Report_Origin; Caller : Iir; Callee : Iir; Loc : Iir)
   is
      L : Iir;
   begin
      if Loc = Null_Iir then
         L := Caller;
      else
         L := Loc;
      end if;
      Error_Msg_Relaxed
        (Origin, Warnid_Pure,
         "pure " & Disp_Node (Caller) & " cannot call (impure) "
         & Disp_Node (Callee), L);
      Error_Msg_Relaxed
        (Origin, Warnid_Pure,
         "(" & Disp_Node (Callee) & " is defined here)", Callee);
   end Error_Pure;

   procedure Error_Not_Match (Expr: Iir; A_Type: Iir) is
   begin
      if Is_Error (A_Type) then
         --  Cascade error message.
         return;
      end if;
      Error_Msg_Sem (+Expr, "can't match %n with type %n", (+Expr, +A_Type));
   end Error_Not_Match;

   function Get_Mode_Name (Mode : Iir_Mode) return String is
   begin
      case Mode is
         when Iir_Unknown_Mode =>
            raise Internal_Error;
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
   end Get_Mode_Name;

   function "+" (V : Iir) return Earg_Type is
   begin
      return Make_Earg_Vhdl_Node (Uns32 (V));
   end "+";

   function "+" (V : Vhdl.Tokens.Token_Type) return Earg_Type is
   begin
      return Make_Earg_Vhdl_Token (Vhdl.Tokens.Token_Type'Pos (V));
   end "+";

   procedure Vhdl_Node_Handler
     (Format : Character; Err : Error_Record; Val : Uns32)
   is
      N : constant Iir := Iir (Val);
   begin
      case Format is
         when 'i' =>
            Output_Quoted_Identifier (Get_Identifier (N));
         when 'l' =>
            Output_Location (Err, Get_Location (N));
         when 'n' =>
            Output_Message (Disp_Node (N));
         when others =>
            raise Internal_Error;
      end case;
   end Vhdl_Node_Handler;

   procedure Vhdl_Token_Handler
     (Format : Character; Err : Error_Record; Val : Uns32)
   is
      pragma Unreferenced (Err);
      use Vhdl.Tokens;
      Tok : constant Token_Type := Token_Type'Val (Val);
   begin
      case Format is
         when 't' =>
            case Tok is
               when Tok_Identifier =>
                  Output_Message ("an identifier");
               when Tok_Eof =>
                  Output_Message ("end of file");
               when others =>
                  Output_Message ("'");
                  Output_Message (Image (Tok));
                  Output_Message ("'");
            end case;
         when others =>
            raise Internal_Error;
      end case;
   end Vhdl_Token_Handler;

   procedure Initialize is
   begin
      Register_Earg_Handler (Earg_Vhdl_Node, Vhdl_Node_Handler'Access);
      Register_Earg_Handler (Earg_Vhdl_Token, Vhdl_Token_Handler'Access);
   end Initialize;
end Vhdl.Errors;
