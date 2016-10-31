--  Semantic analysis.
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
with Iirs_Utils; use Iirs_Utils;
with Sem_Expr; use Sem_Expr;
with Sem_Names; use Sem_Names;
with Evaluation; use Evaluation;
with Std_Package; use Std_Package;
with Errorout; use Errorout;
with Sem; use Sem;
with Sem_Scopes; use Sem_Scopes;
with Sem_Assocs; use Sem_Assocs;
with Libraries;
with Iir_Chains; use Iir_Chains;
with Flags; use Flags;
with Std_Names;
with Sem_Decls;
with Xrefs; use Xrefs;
with Back_End;

package body Sem_Specs is
   function Get_Entity_Class_Kind (Decl : Iir) return Tokens.Token_Type
   is
      use Tokens;
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Entity_Declaration =>
            return Tok_Entity;
         when Iir_Kind_Architecture_Body =>
            return Tok_Architecture;
         when Iir_Kind_Configuration_Declaration =>
            return Tok_Configuration;
         when Iir_Kind_Package_Declaration =>
            return Tok_Package;
         when Iir_Kind_Procedure_Declaration =>
            return Tok_Procedure;
         when Iir_Kind_Function_Declaration =>
            return Tok_Function;
         when Iir_Kind_Type_Declaration =>
            return Tok_Type;
         when Iir_Kind_Subtype_Declaration =>
            return Tok_Subtype;
         when Iir_Kind_Constant_Declaration
           | Iir_Kind_Interface_Constant_Declaration =>
            return Tok_Constant;
         when Iir_Kind_Signal_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Guard_Signal_Declaration =>
            return Tok_Signal;
         when Iir_Kind_Variable_Declaration
           | Iir_Kind_Interface_Variable_Declaration =>
            return Tok_Variable;
         when Iir_Kind_Component_Declaration =>
            return Tok_Component;
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Component_Instantiation_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kinds_Sequential_Statement =>
            return Tok_Label;
         when Iir_Kind_Enumeration_Literal =>
            return Tok_Literal;
         when Iir_Kind_Unit_Declaration =>
            return Tok_Units;
         when Iir_Kind_Group_Declaration =>
            return Tok_Group;
         when Iir_Kind_File_Declaration
           | Iir_Kind_Interface_File_Declaration =>
            return Tok_File;
         when Iir_Kind_Attribute_Declaration =>
            --  Even if an attribute can't have a attribute...
            --  Because an attribute declaration can appear in a declaration
            --  region.
            return Tok_Attribute;
         when others =>
            Error_Kind ("get_entity_class_kind", Decl);
      end case;
      return Tok_Invalid;
   end Get_Entity_Class_Kind;

   --  Return the node containing the attribute_value_chain field for DECL.
   --  This is the parent of the attribute specification, so in general this
   --  is also the parent of the declaration, but there are exceptions...
   function Get_Attribute_Value_Chain_Parent (Decl : Iir) return Iir
   is
      Parent : Iir;
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Entity_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Architecture_Body
           | Iir_Kind_Configuration_Declaration =>
            --  LRM93 5.1
            --  An attribute specification for an attribute of a design unit
            --  [...] must appear immediately within the declarative part of
            --  that design unit.
            return Decl;
         when Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration =>
            --  LRM93 5.1
            --  Similarly, an attribute specification for an attribute of an
            --  interface object of a design unit, subprogram, block statement
            --  or package  must appear immediately within the declarative part
            --  of that design unit, subprogram, block statement, or package.
            Parent := Get_Parent (Decl);
            case Get_Kind (Parent) is
               when Iir_Kind_Entity_Declaration
                 | Iir_Kind_Block_Statement
                 | Iir_Kind_Package_Declaration
                 | Iir_Kind_Package_Instantiation_Declaration =>
                  return Parent;
               when Iir_Kind_Procedure_Declaration
                 | Iir_Kind_Function_Declaration =>
                  return Get_Subprogram_Body (Parent);
               when others =>
                  raise Internal_Error;
            end case;
         when Iir_Kinds_Sequential_Statement =>
            --  Sequential statements can be nested.
            Parent := Get_Parent (Decl);
            loop
               if Get_Kind (Parent) not in Iir_Kinds_Sequential_Statement then
                  return Parent;
               end if;
               Parent := Get_Parent (Parent);
            end loop;
         when others =>
            --  This is also true for enumeration literals and physical units.
            return Get_Parent (Decl);
      end case;
   end Get_Attribute_Value_Chain_Parent;

   function Find_Attribute_Value (Ent : Iir; Id : Name_Id) return Iir
   is
      Attr_Value_Parent : constant Iir :=
        Get_Attribute_Value_Chain_Parent (Ent);
      Value : Iir;
      Spec : Iir;
      Attr_Decl : Iir;
   begin
      Value := Get_Attribute_Value_Chain (Attr_Value_Parent);
      while Value /= Null_Iir loop
         if Get_Designated_Entity (Value) = Ent then
            Spec := Get_Attribute_Specification (Value);
            Attr_Decl := Get_Attribute_Designator (Spec);
            if Get_Identifier (Attr_Decl) = Id then
               return Value;
            end if;
         end if;
         Value := Get_Value_Chain (Value);
      end loop;
      return Null_Iir;
   end Find_Attribute_Value;

   --  Decorate DECL with attribute ATTR.
   --  If CHECK_CLASS is true, class of DECL must be class of ATTR, otherwise
   --   returns silently.
   --  If CHECK_DEFINED is true, DECL must not have been decorated, otherwise
   --   returns silently.
   procedure Attribute_A_Decl (Decl : Iir;
                               Attr : Iir_Attribute_Specification;
                               Check_Class : Boolean;
                               Check_Defined : Boolean)
   is
      use Tokens;
      Attr_Expr : constant Iir := Get_Expression (Attr);

      El : Iir_Attribute_Value;

      --  Attribute declaration corresponding to ATTR.
      --  Due to possible error, it is not required to be an attribute decl,
      --  it may be a simple name.
      Attr_Decl : Iir;

      Attr_Chain_Parent : Iir;
   begin
      --  LRM93 5.1
      --  It is an error if the class of those names is not the same as that
      --  denoted by the entity class.
      if Get_Entity_Class_Kind (Decl) /= Get_Entity_Class (Attr) then
         if Check_Class then
            Error_Msg_Sem (+Attr, "%n is not of class %t",
                           (+Decl, +Get_Entity_Class (Attr)));
            if Get_Kind (Decl) = Iir_Kind_Subtype_Declaration
              and then Get_Entity_Class (Attr) = Tok_Type
              and then Get_Type (Decl) /= Null_Iir
              and then Get_Base_Type (Get_Type (Decl)) /= Null_Iir
              and then Get_Kind
              (Get_Type_Declarator (Get_Base_Type (Get_Type (Decl))))
              = Iir_Kind_Anonymous_Type_Declaration
            then
               --  The type declaration declares an anonymous type
               --  and a named subtype.
               Error_Msg_Sem
                 (+Decl,
                  "%i declares both an anonymous type and a named subtype",
                  +Decl);
            end if;
         end if;
         return;
      end if;

      --  LRM93 5.1
      --  An attribute specification for an attribute of a design unit
      --  (ie an entity declaration, an architecture, a configuration, or a
      --  package) must appear immediately within the declarative part of
      --  that design unit.
      case Get_Entity_Class (Attr) is
         when Tok_Entity
           | Tok_Architecture
           | Tok_Configuration
           | Tok_Package =>
            if Get_Design_Unit (Decl) /= Get_Current_Design_Unit then
               Error_Msg_Sem (+Attr, "%n must appear immediatly within %n",
                              (+Attr, +Decl));
               return;
            end if;
         when others =>
            null;
      end case;

      Attr_Decl := Get_Named_Entity (Get_Attribute_Designator (Attr));

      --  LRM93 5.1
      --  It is an error if a given attribute is associated more than once with
      --  a given named entity.
      --  LRM 5.1
      --  Similarly, it is an error if two different attributes with the
      --  same simple name (wether predefined or user-defined) are both
      --  associated with a given named entity.
      Attr_Chain_Parent := Get_Attribute_Value_Chain_Parent (Decl);
      El := Get_Attribute_Value_Chain (Attr_Chain_Parent);
      while El /= Null_Iir loop
         if Get_Designated_Entity (El) = Decl then
            declare
               El_Attr : constant Iir_Attribute_Declaration :=
                 Get_Named_Entity (Get_Attribute_Designator
                                     (Get_Attribute_Specification (El)));
            begin
               if El_Attr = Attr_Decl then
                  if Get_Attribute_Specification (El) = Attr then
                     --  Was already specified with the same attribute value.
                     --  This is possible only in one case:
                     --
                     --    signal S1       : real;
                     --    alias  S1_too   : real is S1;
                     --    attribute ATTR : T1;
                     --    attribute ATTR of ALL : signal is '1';
                     return;
                  end if;
                  if Check_Defined then
                     Error_Msg_Sem
                       (+Attr, "%n has already %n", (+Decl, +Attr),
                        Cont => True);
                     Error_Msg_Sem
                       (+Attr, "previous attribute specification at %l", +El);
                  end if;
                  return;
               elsif Get_Identifier (El_Attr) = Get_Identifier (Attr_Decl) then
                  Error_Msg_Sem (+Attr, "%n is already decorated with an %n",
                                 (+Decl, +El_Attr), Cont => True);
                  Error_Msg_Sem
                    (+El, "(previous attribute specification was here)");
                  return;
               end if;
            end;
         end if;
         El := Get_Value_Chain (El);
      end loop;

      El := Create_Iir (Iir_Kind_Attribute_Value);
      Location_Copy (El, Attr);
      Set_Name_Staticness (El, None);
      Set_Attribute_Specification (El, Attr);
      --  FIXME: create an expr_error node?
      if Is_Error (Attr_Expr) then
         Set_Expr_Staticness (El, Locally);
      else
         Set_Expr_Staticness (El, Get_Expr_Staticness (Attr_Expr));
      end if;
      Set_Designated_Entity (El, Decl);
      Set_Type (El, Get_Type (Attr_Expr));
      Set_Base_Name (El, El);

      --  Put the attribute value in the attribute_value_chain.
      Set_Value_Chain (El, Get_Attribute_Value_Chain (Attr_Chain_Parent));
      Set_Attribute_Value_Chain (Attr_Chain_Parent, El);

      --  Put the attribute value in the chain of the attribute specification.
      --  This is prepended, so in reverse order.  Will be reversed later.
      Set_Spec_Chain (El, Get_Attribute_Value_Spec_Chain (Attr));
      Set_Attribute_Value_Spec_Chain (Attr, El);

      --  Special handling for 'Foreign.
      if (Flags.Vhdl_Std >= Vhdl_93c
          and then Attr_Decl = Foreign_Attribute)
        or else
        (Flags.Vhdl_Std <= Vhdl_93c
         and then Get_Identifier (Attr_Decl) = Std_Names.Name_Foreign)
      then
         --  LRM93 12.4
         --  The 'FOREIGN attribute may be associated only with
         --  architectures or with subprograms.
         case Get_Entity_Class (Attr) is
            when Tok_Architecture =>
               null;

            when Tok_Function
              | Tok_Procedure =>
               --  LRM93 12.4
               --  In the latter case, the attribute specification must
               --  appear in the declarative part in which the subprogram
               --  is declared.
               --  GHDL: huh, this is the case for any attributes.
               null;

            when others =>
               Error_Msg_Sem
                 (+Attr,
                  "'FOREIGN allowed only for architectures and subprograms");
               return;
         end case;

         Set_Foreign_Flag (Decl, True);

         --  Use 'standard' convention call for foreign procedures, so as a
         --  consequence they cannot be suspended.
         if Get_Kind (Decl) = Iir_Kind_Procedure_Declaration then
            Set_Suspend_Flag (Decl, False);
         end if;

         declare
            use Back_End;
         begin
            if Sem_Foreign /= null then
               Sem_Foreign.all (Decl);
            end if;
         end;
      end if;
   end Attribute_A_Decl;

   --  IS_DESIGNATORS if true if the entity name list is a list of designators.
   --  Return TRUE if an entity was attributed.
   function Sem_Named_Entities
     (Scope : Iir;
      Name : Iir;
      Attr : Iir_Attribute_Specification;
      Is_Designators : Boolean;
      Check_Defined : Boolean)
     return Boolean
   is
      Res : Boolean;

      --  If declaration DECL matches then named entity ENT, apply attribute
      --  specification and returns TRUE. Otherwise, return FALSE.
      --  Note: ENT and DECL are different for aliases.
      function Sem_Named_Entity1 (Ent : Iir; Decl : Iir) return Boolean
      is
         Ent_Id : constant Name_Id := Get_Identifier (Ent);
      begin
         if (Name = Null_Iir or else Ent_Id = Get_Identifier (Name))
           and then Ent_Id /= Null_Identifier
         then
            if Is_Designators then
               Xref_Ref (Name, Ent);
            end if;
            if Get_Visible_Flag (Ent) = False then
               Error_Msg_Sem (+Attr, "%n is not yet visible", +Ent);
            else
               Attribute_A_Decl (Decl, Attr, Is_Designators, Check_Defined);
               return True;
            end if;
         end if;
         return False;
      end Sem_Named_Entity1;

      procedure Sem_Named_Entity (Ent : Iir) is
      begin
         case Get_Kind (Ent) is
            when Iir_Kinds_Library_Unit_Declaration
              | Iir_Kinds_Concurrent_Statement
              | Iir_Kinds_Sequential_Statement
              | Iir_Kinds_Non_Alias_Object_Declaration
              | Iir_Kind_Type_Declaration
              | Iir_Kind_Subtype_Declaration
              | Iir_Kind_Component_Declaration
              | Iir_Kind_Enumeration_Literal
              | Iir_Kind_Unit_Declaration
              | Iir_Kind_Group_Template_Declaration
              | Iir_Kind_Group_Declaration =>
               Res := Res or Sem_Named_Entity1 (Ent, Ent);
            when Iir_Kind_Function_Declaration
              | Iir_Kind_Procedure_Declaration =>
               if not Is_Second_Subprogram_Specification (Ent) then
                  Res := Res or Sem_Named_Entity1 (Ent, Ent);
               end if;
            when Iir_Kind_Object_Alias_Declaration =>
               --  LRM93 5.1
               --  An entity designator that denotes an alias of an object is
               --  required to denote the entire object, and not a subelement
               --  or slice thereof.
               declare
                  Decl : constant Iir := Get_Name (Ent);
                  Base : constant Iir := Get_Object_Prefix (Decl, False);
                  Applied : Boolean;
               begin
                  Applied := Sem_Named_Entity1 (Ent, Base);
                  --  FIXME: check the alias denotes a local entity...
                  if Applied
                    and then Base /= Strip_Denoting_Name (Decl)
                  then
                     Error_Msg_Sem
                       (+Attr, "%n does not denote the entire object", +Ent);
                  end if;
                  Res := Res or Applied;
               end;
            when Iir_Kind_Non_Object_Alias_Declaration =>
               Res := Res
                 or Sem_Named_Entity1 (Ent, Get_Named_Entity (Get_Name (Ent)));
            when Iir_Kind_Attribute_Declaration
              | Iir_Kind_Attribute_Specification
              | Iir_Kind_Configuration_Specification
              | Iir_Kind_Use_Clause =>
               null;
            when Iir_Kind_Procedure_Body
              | Iir_Kind_Function_Body =>
               null;
            when Iir_Kind_Anonymous_Type_Declaration =>
               null;
            when others =>
               Error_Kind ("sem_named_entity", Ent);
         end case;
      end Sem_Named_Entity;

      procedure Sem_Named_Entity_Chain (Chain_First : Iir)
      is
         El : Iir;
         Def : Iir;
      begin
         El := Chain_First;
         while El /= Null_Iir loop
            exit when El = Attr;
            Sem_Named_Entity (El);
            case Get_Kind (El) is
               when Iir_Kind_Type_Declaration =>
                  Def := Get_Type_Definition (El);
                  if Get_Kind (Def) = Iir_Kind_Enumeration_Type_Definition then
                     declare
                        List : Iir_List;
                        El1 : Iir;
                     begin
                        List := Get_Enumeration_Literal_List (Def);
                        for I in Natural loop
                           El1 := Get_Nth_Element (List, I);
                           exit when El1 = Null_Iir;
                           Sem_Named_Entity (El1);
                        end loop;
                     end;
                  end if;
               when Iir_Kind_Anonymous_Type_Declaration =>
                  Def := Get_Type_Definition (El);
                  if Get_Kind (Def) = Iir_Kind_Physical_Type_Definition then
                     declare
                        El1 : Iir;
                     begin
                        El1 := Get_Unit_Chain (Def);
                        while El1 /= Null_Iir loop
                           Sem_Named_Entity (El1);
                           El1 := Get_Chain (El1);
                        end loop;
                     end;
                  end if;
               when Iir_Kind_For_Loop_Statement
                 | Iir_Kind_While_Loop_Statement =>
                  Sem_Named_Entity_Chain (Get_Sequential_Statement_Chain (El));
               when Iir_Kind_If_Statement =>
                  declare
                     Clause : Iir;
                  begin
                     Clause := El;
                     while Clause /= Null_Iir loop
                        Sem_Named_Entity_Chain
                          (Get_Sequential_Statement_Chain (Clause));
                        Clause := Get_Else_Clause (Clause);
                     end loop;
                  end;
               when Iir_Kind_Case_Statement =>
                  declare
                     El1 : Iir;
                  begin
                     El1 := Get_Case_Statement_Alternative_Chain (El);
                     while El1 /= Null_Iir loop
                        Sem_Named_Entity_Chain (Get_Associated_Chain (El1));
                        El1 := Get_Chain (El1);
                     end loop;
                  end;

               when Iir_Kind_If_Generate_Statement
                 | Iir_Kind_For_Generate_Statement =>
                  --  INT-1991/issue 27
                  --  Generate statements represent declarative region and
                  --  have implicit declarative parts.
                  --  Was: There is no declarative part in generate statement
                  --  for VHDL 87.
                  if False and then Flags.Vhdl_Std = Vhdl_87 then
                     Sem_Named_Entity_Chain
                       (Get_Concurrent_Statement_Chain (El));
                  end if;

               when others =>
                  null;
            end case;
            El := Get_Chain (El);
         end loop;
      end Sem_Named_Entity_Chain;
   begin
      Res := False;

      --  LRM 5.1  Attribute specification
      --  o If a list of entity designators is supplied, then the
      --  attribute specification applies to the named entities denoted
      --  by those designators.
      --
      --  o If the reserved word OTHERS is supplied, then the attribute
      --  specification applies to named entities of the specified class
      --  that are declared in the immediately enclosing declarative
      --  part [...]
      --
      --  o If the reserved word ALL is supplied, then the attribute
      --  specification applies to all named entities of the specified
      --  class that are declared in the immediatly enclosing
      --  declarative part.

      --  NOTE: therefore, ALL/OTHERS do not apply to named entities declared
      --  beyond the immediate declarative part, such as design unit or
      --  interfaces.
      if Is_Designators then
         --  LRM 5.1  Attribute specification
         --  An attribute specification for an attribute of a design unit
         --  (i.e. an entity declaration, an architecture, a configuration
         --  or a package) must appear immediatly within the declarative part
         --  of that design unit.
         case Get_Kind (Scope) is
            when Iir_Kind_Entity_Declaration
              | Iir_Kind_Architecture_Body
              | Iir_Kind_Configuration_Declaration
              | Iir_Kind_Package_Declaration =>
               Sem_Named_Entity (Scope);
            when others =>
               null;
         end case;

         --  LRM 5.1  Attribute specification
         --  Similarly, an attribute specification for an attribute of an
         --  interface object of a design unit, subprogram or block statement
         --  must appear immediatly within the declarative part of that design
         --  unit, subprogram, or block statement.
         case Get_Kind (Scope) is
            when Iir_Kind_Entity_Declaration =>
               Sem_Named_Entity_Chain (Get_Generic_Chain (Scope));
               Sem_Named_Entity_Chain (Get_Port_Chain (Scope));
            when Iir_Kind_Block_Statement =>
               declare
                  Header : constant Iir := Get_Block_Header (Scope);
               begin
                  if Header /= Null_Iir then
                     Sem_Named_Entity_Chain (Get_Generic_Chain (Header));
                     Sem_Named_Entity_Chain (Get_Port_Chain (Header));
                  end if;
               end;
            when Iir_Kind_Function_Body
              | Iir_Kind_Procedure_Body =>
               declare
                  Spec : Iir;
               begin
                  Spec := Get_Subprogram_Specification (Scope);
                  Sem_Named_Entity_Chain
                    (Get_Interface_Declaration_Chain (Spec));
               end;
            when others =>
               null;
         end case;
      end if;

      case Get_Kind (Scope) is
         when Iir_Kind_Entity_Declaration
           | Iir_Kind_Architecture_Body
           | Iir_Kind_Generate_Statement_Body =>
            Sem_Named_Entity_Chain (Get_Declaration_Chain (Scope));
            Sem_Named_Entity_Chain (Get_Concurrent_Statement_Chain (Scope));
         when Iir_Kind_Block_Statement =>
            declare
               Guard : constant Iir := Get_Guard_Decl (Scope);
            begin
               if Guard /= Null_Iir then
                  Sem_Named_Entity (Guard);
               end if;
            end;
            Sem_Named_Entity_Chain (Get_Declaration_Chain (Scope));
            Sem_Named_Entity_Chain (Get_Concurrent_Statement_Chain (Scope));
         when Iir_Kind_Configuration_Declaration =>
            null;
         when Iir_Kind_Package_Declaration =>
            Sem_Named_Entity_Chain (Get_Declaration_Chain (Scope));
         when Iir_Kinds_Process_Statement =>
            Sem_Named_Entity_Chain (Get_Declaration_Chain (Scope));
            Sem_Named_Entity_Chain (Get_Sequential_Statement_Chain (Scope));
         when Iir_Kind_Package_Body =>
            Sem_Named_Entity_Chain (Get_Declaration_Chain (Scope));
         when Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body =>
            Sem_Named_Entity_Chain (Get_Declaration_Chain (Scope));
            Sem_Named_Entity_Chain (Get_Sequential_Statement_Chain (Scope));
         when others =>
            Error_Kind ("sem_named_entities", Scope);
      end case;
      return Res;
   end Sem_Named_Entities;

   procedure Sem_Signature_Entity_Designator
     (Sig : Iir_Signature; Attr : Iir_Attribute_Specification)
   is
      Prefix : Iir;
      Inter : Name_Interpretation_Type;
      List : Iir_List;
      Name : Iir;
   begin
      List := Create_Iir_List;

      --  Sem_Name cannot be used here (at least not directly) because only
      --  the declarations of the current scope are considered.
      Prefix := Get_Signature_Prefix (Sig);
      Inter := Get_Interpretation (Get_Identifier (Prefix));
      while Valid_Interpretation (Inter) loop
         exit when not Is_In_Current_Declarative_Region (Inter);
         if not Is_Potentially_Visible (Inter) then
            Name := Get_Declaration (Inter);
            --  LRM 5.1 Attribute Specification
            --  The entity tag of an entity designator containing a signature
            --  must denote the name of one or more subprograms or enumeration
            --  literals.
            case Get_Kind (Name) is
               when Iir_Kind_Function_Declaration
                 | Iir_Kind_Procedure_Declaration
                 | Iir_Kind_Enumeration_Literal =>
                  Append_Element (List, Name);
               when others =>
                  Error_Msg_Sem
                    (+Sig, "entity tag must denote a subprogram or a literal");
            end case;
         end if;
         Inter := Get_Next_Interpretation (Inter);
      end loop;

      Name := Sem_Decls.Sem_Signature (Create_Overload_List (List), Sig);
      if Name = Null_Iir then
         return;
      end if;

      Set_Named_Entity (Prefix, Name);
      Prefix := Finish_Sem_Name (Prefix);
      Set_Signature_Prefix (Sig, Prefix);

      Attribute_A_Decl (Name, Attr, True, True);
   end Sem_Signature_Entity_Designator;

   procedure Sem_Attribute_Specification
     (Spec : Iir_Attribute_Specification; Scope : Iir)
   is
      use Tokens;

      Name : Iir;
      Attr : Iir_Attribute_Declaration;
      Attr_Type : Iir;
      List : Iir_List;
      Expr : Iir;
      Res : Boolean;
   begin
      --  LRM93 5.1
      --  The attribute designator must denote an attribute.
      Name := Sem_Denoting_Name (Get_Attribute_Designator (Spec));
      Set_Attribute_Designator (Spec, Name);

      Attr := Get_Named_Entity (Name);
      if Get_Kind (Attr) /= Iir_Kind_Attribute_Declaration then
         Error_Class_Match (Name, "attribute");
         return;
      end if;

      --  LRM 5.1
      --  The type of the expression in the attribute specification must be
      --  the same as (or implicitly convertible to) the type mark in the
      --  corresponding attribute declaration.
      Attr_Type := Get_Type (Attr);
      Expr := Sem_Expression (Get_Expression (Spec), Attr_Type);
      if Expr /= Null_Iir then
         Check_Read (Expr);
         Expr := Eval_Expr_If_Static (Expr);
         Set_Expression (Spec, Expr);

         --  LRM 5.1
         --  If the entity name list denotes an entity declaration,
         --  architecture body or configuration declaration, then the
         --  expression is required to be locally static.
         --  GHDL: test based on the entity_class.
         case Get_Entity_Class (Spec) is
            when Tok_Entity
              | Tok_Architecture
              | Tok_Configuration =>
               if Get_Expr_Staticness (Expr) /= Locally then
                  Error_Msg_Sem
                    (+Spec,
                     "attribute expression for %t must be locally static",
                     +Get_Entity_Class (Spec));
               end if;
            when others =>
               null;
         end case;
      else
         Set_Expression
           (Spec, Create_Error_Expr (Get_Expression (Spec), Attr_Type));
      end if;

      --  LRM93 3.2.1.1 Index constraints and discrete ranges
      --  - For an attribute whose value is specified by an attribute
      --    specification, the index ranges are defined by the expression
      --    given in the specification, if the subtype of the attribute is
      --    unconstrained [...]
      --  GHDL: For attribute value.

      --  LRM 5.1
      --  The entity name list identifies those named entities, both
      --  implicitly and explicitly defined, that inherit the attribute, as
      --  defined below:
      List := Get_Entity_Name_List (Spec);
      if List = Iir_List_All then
         --  o If the reserved word ALL is supplied, then the attribute
         --  specification applies to all named entities of the specified
         --  class that are declared in the immediatly enclosing
         --  declarative part.
         Res := Sem_Named_Entities (Scope, Null_Iir, Spec, False, True);
         if Res = False and then Is_Warning_Enabled (Warnid_Specs) then
            Warning_Msg_Sem
              (Warnid_Specs, +Spec,
               "attribute specification apply to no named entity");
         end if;
      elsif List = Iir_List_Others then
         --  o If the reserved word OTHERS is supplied, then the attribute
         --  specification applies to named entities of the specified class
         --  that are declared in the immediately enclosing declarative
         --  part, provided that each such entity is not explicitly named
         --  in the entity name list of a previous attribute specification
         --  for the given attribute.
         Res := Sem_Named_Entities (Scope, Null_Iir, Spec, False, False);
         if Res = False and then Is_Warning_Enabled (Warnid_Specs) then
            Warning_Msg_Sem
              (Warnid_Specs, +Spec,
               "attribute specification apply to no named entity");
         end if;
      else
         --  o If a list of entity designators is supplied, then the
         --  attribute specification applies to the named entities denoted
         --  by those designators.
         declare
            El : Iir;
         begin
            for I in Natural loop
               El := Get_Nth_Element (List, I);
               exit when El = Null_Iir;
               if Get_Kind (El) = Iir_Kind_Signature then
                  Sem_Signature_Entity_Designator (El, Spec);
               else
                  --  LRM 5.1
                  --  It is an error if the class of those names is not the
                  --  same as that denoted by entity class.
                  if not Sem_Named_Entities (Scope, El, Spec, True, True) then
                     Error_Msg_Sem
                       (+El, "no named entities %i in declarative part", +El);
                  end if;
               end if;
            end loop;
         end;
      end if;

      --  Reverse the chain of attribute value in specification, so that they
      --  are in textual order.  This is important if the expression is not
      --  static.
      declare
         El : Iir;
         New_El : Iir;
         Tmp : Iir;
      begin
         El := Get_Attribute_Value_Spec_Chain (Spec);
         New_El := Null_Iir;
         while Is_Valid (El) loop
            Tmp := Get_Spec_Chain (El);
            Set_Spec_Chain (El, New_El);
            New_El := El;
            El := Tmp;
         end loop;
         Set_Attribute_Value_Spec_Chain (Spec, New_El);
      end;
   end Sem_Attribute_Specification;

   procedure Check_Post_Attribute_Specification
     (Attr_Spec_Chain : Iir; Decl : Iir)
   is
      use Tokens;

      Has_Error : Boolean;
      Spec : Iir;
      Decl_Class : Token_Type;
      Decl_Class2 : Token_Type;
      Ent_Class : Token_Type;
   begin
      --  Some declaration items can never be attributed.
      Decl_Class2 := Tok_Eof;
      case Get_Kind (Decl) is
         when Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Use_Clause
           | Iir_Kind_Attribute_Declaration
           | Iir_Kinds_Signal_Attribute
           | Iir_Kind_Disconnection_Specification =>
            return;
         when Iir_Kind_Anonymous_Type_Declaration =>
            --  A physical type definition declares units.
            if Get_Kind (Get_Type_Definition (Decl))
              = Iir_Kind_Physical_Type_Definition
            then
               Decl_Class := Tok_Units;
            else
               return;
            end if;
         when Iir_Kind_Attribute_Specification =>
            Decl_Class := Get_Entity_Class (Decl);
         when Iir_Kind_Type_Declaration =>
            Decl_Class := Tok_Type;
            --  An enumeration type declares literals.
            if Get_Kind (Get_Type_Definition (Decl))
              = Iir_Kind_Enumeration_Type_Definition
            then
               Decl_Class2 := Tok_Literal;
            end if;
         when Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Object_Alias_Declaration =>
            Decl_Class := Get_Entity_Class_Kind (Get_Name (Decl));
            --  NOTE: for non-object alias that declares an enumeration type
            --  or a physical type, no need to set decl_class2, since
            --  all implicit aliases are checked.
         when others =>
            Decl_Class := Get_Entity_Class_Kind (Decl);
      end case;

      Spec := Attr_Spec_Chain;
      --  Skip itself (newly added, therefore first of the chain).
      if Spec = Decl then
         Spec := Get_Attribute_Specification_Chain (Spec);
      end if;
      while Spec /= Null_Iir loop
         pragma Assert (Get_Entity_Name_List (Spec) in Iir_Lists_All_Others);
         Ent_Class := Get_Entity_Class (Spec);
         if Ent_Class = Decl_Class or Ent_Class = Decl_Class2 then
            Has_Error := False;

            if Get_Kind (Decl) = Iir_Kind_Attribute_Specification then
               --  LRM 5.1  Attribute specifications
               --  An attribute specification with the entity name list OTHERS
               --  or ALL for a given entity class that appears in a
               --  declarative part must be the last such specification for the
               --  given attribute for the given entity class in that
               --  declarative part.
               if Get_Identifier (Get_Attribute_Designator (Decl))
                 = Get_Identifier (Get_Attribute_Designator (Spec))
               then
                  Error_Msg_Sem
                    (+Decl, "no attribute specification may follow an "
                       & "all/others spec", Cont => True);
                  Has_Error := True;
               end if;
            else
               --  LRM 5.1  Attribute specifications
               --  It is an error if a named entity in the specificied entity
               --  class is declared in a given declarative part following such
               --  an attribute specification.
               Error_Msg_Sem
                 (+Decl, "no named entity may follow an all/others attribute "
                    & "specification", Cont => True);
               Has_Error := True;
            end if;
            if Has_Error then
               Error_Msg_Sem
                 (+Spec, "(previous all/others specification for the given "
                    &"entity class)");
            end if;
         end if;
         Spec := Get_Attribute_Specification_Chain (Spec);
      end loop;
   end Check_Post_Attribute_Specification;

   --  Compare ATYPE and TYPE_MARK.
   --  ATYPE is a type definition, which can be anonymous.
   --  TYPE_MARK is a subtype definition, established from a type mark.
   --   Therefore, it is the name of a type or a subtype.
   --  Return TRUE iff the type mark of ATYPE is TYPE_MARK.
   function Is_Same_Type_Mark (Atype : Iir; Type_Mark : Iir)
                              return Boolean is
   begin
      if Get_Kind (Atype) in Iir_Kinds_Subtype_Definition
        and then Is_Anonymous_Type_Definition (Atype)
      then
         --  FIXME: to be removed; used to catch uninitialized type_mark.
         if Get_Subtype_Type_Mark (Atype) = Null_Iir then
            raise Internal_Error;
         end if;
         return Get_Type (Get_Subtype_Type_Mark (Atype)) = Type_Mark;
      else
         return Atype = Type_Mark;
      end if;
   end Is_Same_Type_Mark;

   procedure Sem_Disconnection_Specification
     (Dis : Iir_Disconnection_Specification)
   is
      Type_Mark : Iir;
      Atype : Iir;
      Time_Expr : Iir;
      List : Iir_List;
      El : Iir;
      Sig : Iir;
      Prefix : Iir;
   begin
      --  Sem type mark.
      Type_Mark := Get_Type_Mark (Dis);
      Type_Mark := Sem_Type_Mark (Type_Mark);
      Set_Type_Mark (Dis, Type_Mark);
      Atype := Get_Type (Type_Mark);

      --  LRM93 5.3
      --  The time expression in a disconnection specification must be static
      --  and must evaluate to a non-negative value.
      Time_Expr := Sem_Expression
        (Get_Expression (Dis), Time_Subtype_Definition);
      if Time_Expr /= Null_Iir then
         Check_Read (Time_Expr);
         Set_Expression (Dis, Time_Expr);
         if Get_Expr_Staticness (Time_Expr) < Globally then
            Error_Msg_Sem (+Time_Expr, "time expression must be static");
         end if;
      end if;

      List := Get_Signal_List (Dis);
      if List = Iir_List_All or List = Iir_List_Others then
         --  FIXME: checks todo
         null;
      else
         for I in Natural loop
            El := Get_Nth_Element (List, I);
            exit when El = Null_Iir;

            Sem_Name (El);
            El := Finish_Sem_Name (El);
            Replace_Nth_Element (List, I, El);

            Sig := Get_Named_Entity (El);
            Sig := Name_To_Object (Sig);
            if Sig /= Null_Iir then
               Set_Type (El, Get_Type (Sig));
               Prefix := Get_Object_Prefix (Sig);
               --  LRM93 5.3
               --  Each signal name in a signal list in a guarded signal
               --  specification must be a locally static name that
               --  denotes a guarded signal.
               case Get_Kind (Prefix) is
                  when Iir_Kind_Signal_Declaration
                    | Iir_Kind_Interface_Signal_Declaration =>
                     null;
                  when others =>
                     Error_Msg_Sem (+El, "object must be a signal");
                     return;
               end case;
               if Get_Name_Staticness (Sig) /= Locally then
                  Error_Msg_Sem (+El, "signal name must be locally static");
               end if;
               if not Get_Guarded_Signal_Flag (Prefix) then
                  Error_Msg_Sem (+El, "signal must be a guarded signal");
               end if;
               Set_Has_Disconnect_Flag (Prefix, True);

               --  LRM93 5.3
               --  If the guarded signal is a declared signal or a slice of
               --  thereof, the type mark must be the same as the type mark
               --  indicated in the guarded signal specification.
               --  If the guarded signal is an array element of an explicitly
               --  declared signal, the type mark must be the same as the
               --  element subtype indication in the (explicit or implicit)
               --  array type declaration that declares the base type of the
               --  explicitly declared signal.
               --  If the guarded signal is a record element of an explicitly
               --  declared signal, then the type mark must be the same as
               --  the type mark in the element subtype definition of the
               --  record type declaration that declares the type of the
               --  explicitly declared signal.
               -- FIXME: to be checked: the expression type (as set by
               --  sem_expression) may be a base type instead of a type mark.
               if not Is_Same_Type_Mark (Get_Type (Sig), Atype) then
                  Error_Msg_Sem (+El, "type mark and signal type mismatch");
               end if;

               --  LRM93 5.3
               --  Each signal must be declared in the declarative part
               --  enclosing the disconnection specification.
               --  FIXME: todo.
            elsif Get_Designated_Entity (El) /= Error_Mark then
               Error_Msg_Sem (+El, "name must designate a signal");
            end if;
         end loop;
      end if;
   end Sem_Disconnection_Specification;

   --  Analyze entity aspect ASPECT and return the entity declaration.
   --  Return NULL_IIR if not found.
   function Sem_Entity_Aspect (Aspect : Iir) return Iir is
   begin
      case Get_Kind (Aspect) is
         when Iir_Kind_Entity_Aspect_Entity =>
            declare
               Entity_Name : Iir;
               Entity : Iir;
               Arch_Name : Iir;
               Arch_Unit : Iir;
            begin
               Entity_Name := Sem_Denoting_Name (Get_Entity_Name (Aspect));
               Set_Entity_Name (Aspect, Entity_Name);
               Entity := Get_Named_Entity (Entity_Name);
               if Entity = Error_Mark then
                  return Null_Iir;
               end if;
               if Get_Kind (Entity) /= Iir_Kind_Entity_Declaration then
                  Error_Class_Match (Entity_Name, "entity");
                  return Null_Iir;
               end if;
               --  Note: dependency is added by Sem_Denoting_Name.

               --  Check architecture.
               Arch_Name := Get_Architecture (Aspect);
               if Arch_Name /= Null_Iir then
                  Arch_Unit := Libraries.Find_Secondary_Unit
                    (Get_Design_Unit (Entity), Get_Identifier (Arch_Name));
                  Set_Named_Entity (Arch_Name, Arch_Unit);
                  if Arch_Unit /= Null_Iir then
                     Xref_Ref (Arch_Name, Arch_Unit);
                  end if;

                  --  FIXME: may emit a warning if the architecture does not
                  --  exist.
                  --  Note: the design needs the architecture.
                  Add_Dependence (Aspect);
               end if;
               return Entity;
            end;

         when Iir_Kind_Entity_Aspect_Configuration =>
            declare
               Conf_Name : Iir;
               Conf : Iir;
            begin
               Conf_Name :=
                 Sem_Denoting_Name (Get_Configuration_Name (Aspect));
               Set_Configuration_Name (Aspect, Conf_Name);
               Conf := Get_Named_Entity (Conf_Name);
               if Is_Error (Conf) then
                  return Null_Iir;
               elsif Get_Kind (Conf) /= Iir_Kind_Configuration_Declaration then
                  Error_Class_Match (Conf, "configuration");
                  return Null_Iir;
               end if;

               return Get_Entity (Conf);
            end;

         when Iir_Kind_Entity_Aspect_Open =>
            return Null_Iir;

         when others =>
            Error_Kind ("sem_entity_aspect", Aspect);
      end case;
   end Sem_Entity_Aspect;

   procedure Sem_Binding_Indication (Bind : Iir_Binding_Indication;
                                     Parent : Iir;
                                     Primary_Entity_Aspect : Iir)
   is
      Entity_Aspect : Iir;
      Entity : Iir_Entity_Declaration;
   begin
      if Bind = Null_Iir then
         raise Internal_Error;
      end if;

      Entity_Aspect := Get_Entity_Aspect (Bind);
      if Entity_Aspect /= Null_Iir then
         Entity := Sem_Entity_Aspect (Entity_Aspect);

         --  LRM93 5.2.1  Binding Indication
         --  An incremental binding indication must not have an entity aspect.
         if Primary_Entity_Aspect /= Null_Iir then
            Error_Msg_Sem
              (+Bind, "entity aspect not allowed for incremental binding");
         end if;

         --  Return now in case of error.
         if Entity = Null_Iir then
            return;
         end if;
      else
         --  LRM93 5.2.1
         --  When a binding indication is used in an explicit configuration
         --  specification, it is an error if the entity aspect is absent.
         case Get_Kind (Parent) is
            when Iir_Kind_Component_Configuration =>
               if Primary_Entity_Aspect = Null_Iir then
                  Entity := Null_Iir;
               else
                  case Get_Kind (Primary_Entity_Aspect) is
                     when Iir_Kind_Entity_Aspect_Entity =>
                        Entity := Get_Entity (Primary_Entity_Aspect);
                     when others =>
                        Error_Kind
                          ("sem_binding_indication", Primary_Entity_Aspect);
                  end case;
               end if;
            when Iir_Kind_Configuration_Specification =>
               Error_Msg_Sem
                 (+Bind,
                  "entity aspect required in a configuration specification");
               return;
            when others =>
               raise Internal_Error;
         end case;
      end if;
      if Entity = Null_Iir
        or else Get_Kind (Entity) = Iir_Kind_Entity_Aspect_Open
      then
         --  LRM 5.2.1.1  Entity aspect
         --  The third form of entity aspect is used to specify that the
         --  indiciation of the design entity is to be defined.  In this case,
         --  the immediatly enclosing binding indication is said to not
         --  imply any design entity.  Furthermore, the immediatly enclosing
         --  binding indication must not include a generic map aspect or a
         --  port map aspect.
         if Get_Generic_Map_Aspect_Chain (Bind) /= Null_Iir
           or else Get_Port_Map_Aspect_Chain (Bind) /= Null_Iir
         then
            Error_Msg_Sem
              (+Bind, "map aspect not allowed for open entity aspect");
            return;
         end if;
      else
         Sem_Generic_Port_Association_Chain (Entity, Bind);

         --  LRM 5.2.1 Binding Indication
         --  If the generic map aspect or port map aspect of a binding
         --  indication is not present, then the default rules as described
         --  in 5.2.2 apply.
         --  GHDL: done in canon
      end if;
   end Sem_Binding_Indication;

   --  Set configuration_specification or component_configuration SPEC to
   --  component instantiation COMP.
   procedure Apply_Configuration_Specification
     (Comp : Iir_Component_Instantiation_Statement;
      Spec : Iir;
      Primary_Entity_Aspect : in out Iir)
   is
      Prev_Spec : Iir;
      Prev_Conf : Iir;

      procedure Prev_Spec_Error is
      begin
         Error_Msg_Sem
           (+Spec, "%n is alreay bound by a configuration specification",
            (1 => +Comp), Cont => True);
         Error_Msg_Sem (+Prev_Spec, "(previous is %n)", +Prev_Spec);
      end Prev_Spec_Error;

      Prev_Binding : Iir_Binding_Indication;
      Prev_Entity_Aspect : Iir;
   begin
      Prev_Spec := Get_Configuration_Specification (Comp);
      if Prev_Spec /= Null_Iir then
         case Get_Kind (Spec) is
            when Iir_Kind_Configuration_Specification =>
               Prev_Spec_Error;
               return;
            when Iir_Kind_Component_Configuration =>
               if Flags.Vhdl_Std = Vhdl_87 then
                  Prev_Spec_Error;
                  Error_Msg_Sem
                    (+Spec, "(incremental binding is not allowed in vhdl87)");
                  return;
               end if;
               --  Incremental binding.
               Prev_Binding := Get_Binding_Indication (Prev_Spec);
               if Prev_Binding /= Null_Iir then
                  Prev_Entity_Aspect := Get_Entity_Aspect (Prev_Binding);
                  if Primary_Entity_Aspect = Null_Iir then
                     Primary_Entity_Aspect := Prev_Entity_Aspect;
                  else
                     --  FIXME: checks to do ?
                     null;
                  end if;
               end if;
            when others =>
               Error_Kind ("apply_configuration_specification", Spec);
         end case;
      end if;
      Prev_Conf := Get_Component_Configuration (Comp);
      if Prev_Conf /= Null_Iir then
         case Get_Kind (Spec) is
            when Iir_Kind_Configuration_Specification =>
               --  How can this happen ?
               raise Internal_Error;
            when Iir_Kind_Component_Configuration =>
               Error_Msg_Sem
                 (+Spec, "%n is already bound by a component configuration",
                  (1 => +Comp), Cont => True);
               Error_Msg_Sem (+Prev_Conf, "(previous is %n)", +Prev_Conf);
               return;
            when others =>
               Error_Kind ("apply_configuration_specification(2)", Spec);
         end case;
      end if;
      if Get_Kind (Spec) = Iir_Kind_Configuration_Specification then
         Set_Configuration_Specification (Comp, Spec);
      end if;
      Set_Component_Configuration (Comp, Spec);
   end Apply_Configuration_Specification;

   --  Analyze component_configuration or configuration_specification SPEC.
   --  STMTS is the concurrent statement list related to SPEC.
   procedure Sem_Component_Specification
     (Parent_Stmts : Iir; Spec : Iir; Primary_Entity_Aspect : out Iir)
   is
      function Apply_Component_Specification
        (Chain : Iir; Check_Applied : Boolean)
        return Boolean
      is
         Comp : constant Iir := Get_Named_Entity (Get_Component_Name (Spec));
         El : Iir;
         Res : Boolean;
      begin
         El := Get_Concurrent_Statement_Chain (Chain);
         Res := False;
         while El /= Null_Iir loop
            case Get_Kind (El) is
               when Iir_Kind_Component_Instantiation_Statement =>
                  if Is_Component_Instantiation (El)
                    and then
                    Get_Named_Entity (Get_Instantiated_Unit (El)) = Comp
                    and then
                    (not Check_Applied
                     or else Get_Component_Configuration (El) = Null_Iir)
                  then
                     Apply_Configuration_Specification
                       (El, Spec, Primary_Entity_Aspect);
                     Res := True;
                  end if;
               when Iir_Kind_For_Generate_Statement
                 | Iir_Kind_If_Generate_Statement =>
                  if False and then Flags.Vhdl_Std = Vhdl_87 then
                     Res := Res
                       or Apply_Component_Specification (El, Check_Applied);
                  end if;
               when others =>
                  null;
            end case;
            El := Get_Chain (El);
         end loop;
         return Res;
      end Apply_Component_Specification;

      List : Iir_List;
      El : Iir;
      Inter : Sem_Scopes.Name_Interpretation_Type;
      Comp : Iir;
      Comp_Name : Iir;
      Inst : Iir;
      Inst_Unit : Iir;
   begin
      Primary_Entity_Aspect := Null_Iir;
      Comp_Name := Sem_Denoting_Name (Get_Component_Name (Spec));
      Set_Component_Name (Spec, Comp_Name);
      Comp := Get_Named_Entity (Comp_Name);
      if Get_Kind (Comp) /= Iir_Kind_Component_Declaration then
         Error_Class_Match (Comp_Name, "component");
         return;
      end if;

      List := Get_Instantiation_List (Spec);
      if List = Iir_List_All then
         --  LRM93 5.2
         --  * If the reserved word ALL is supplied, then the configuration
         --    specification applies to all instances of the specified
         --    component declaration whose labels are (implicitly) declared
         --    in the immediately enclosing declarative region part.
         --    This rule applies only to those component instantiation
         --    statements whose corresponding instantiated units name
         --    component.
         if not Apply_Component_Specification (Parent_Stmts, False)
           and then Is_Warning_Enabled (Warnid_Specs)
         then
            Warning_Msg_Sem (Warnid_Specs, +Spec,
                             "component specification applies to no instance");
         end if;
      elsif List = Iir_List_Others then
         --  LRM93 5.2
         --  * If the reserved word OTHERS is supplied, then the
         --    configuration specification applies to instances of the
         --    specified component declaration whoce labels are (implicitly)
         --    declared in the immediatly enclosing declarative part,
         --    provided that each such component instance is not explicitly
         --    names in the instantiation list of a previous configuration
         --    specification.
         --    This rule applies only to those component instantiation
         --    statements whose corresponding instantiated units name
         --    components.
         if not Apply_Component_Specification (Parent_Stmts, True)
           and then Is_Warning_Enabled (Warnid_Specs)
         then
            Warning_Msg_Sem (Warnid_Specs, +Spec,
                             "component specification applies to no instance");
         end if;
      else
         --  LRM93 5.2
         --  * If a list of instantiation labels is supplied, then the
         --    configuration specification applies to the corresponding
         --    component instances.
         --    Such labels must be (implicitly) declared within the
         --    immediatly enclosing declarative part.
         --    It is an error if these component instances are not instances
         --    of the component declaration named in the component
         --    specification.
         --    It is also an error if any of the labels denote a component
         --    instantiation statement whose corresponding instantiated unit
         --    does not name a component.
         -- FIXME: error message are *really* cryptic.
         for I in Natural loop
            El := Get_Nth_Element (List, I);
            exit when El = Null_Iir;
            Inter := Sem_Scopes.Get_Interpretation (Get_Identifier (El));
            if not Valid_Interpretation (Inter) then
               Error_Msg_Sem
                 (+El, "no component instantation with label %i", +El);
            elsif not Is_In_Current_Declarative_Region (Inter) then
               --  FIXME.
               Error_Msg_Sem (+El, "label not in block declarative part");
            else
               Inst := Get_Declaration (Inter);
               if Get_Kind (Inst) /= Iir_Kind_Component_Instantiation_Statement
               then
                  Error_Msg_Sem
                    (+El, "label does not denote an instantiation");
               else
                  Inst_Unit := Get_Instantiated_Unit (Inst);
                  if Is_Entity_Instantiation (Inst)
                    or else (Get_Kind (Get_Named_Entity (Inst_Unit))
                               /= Iir_Kind_Component_Declaration)
                  then
                     Error_Msg_Sem
                       (+El, "specification does not apply to "
                          & "direct instantiation");
                  elsif Get_Named_Entity (Inst_Unit) /= Comp then
                     Error_Msg_Sem (+El, "component names mismatch");
                  else
                     Apply_Configuration_Specification
                       (Inst, Spec, Primary_Entity_Aspect);
                     Xref_Ref (El, Inst);
                     Set_Named_Entity (El, Inst);
                     Set_Is_Forward_Ref (El, True);
                  end if;
               end if;
            end if;
         end loop;
      end if;
   end Sem_Component_Specification;

   procedure Sem_Configuration_Specification
     (Parent_Stmts : Iir; Conf : Iir_Configuration_Specification)
   is
      Primary_Entity_Aspect : Iir;
      Component : Iir;
   begin
      Sem_Component_Specification (Parent_Stmts, Conf, Primary_Entity_Aspect);
      Component := Get_Named_Entity (Get_Component_Name (Conf));

      --  Return now in case of error.
      if Get_Kind (Component) /= Iir_Kind_Component_Declaration then
         return;
      end if;
      --  Extend scope of component interface declaration.
      Sem_Scopes.Open_Scope_Extension;
      Sem_Scopes.Add_Component_Declarations (Component);
      Sem_Binding_Indication
        (Get_Binding_Indication (Conf), Conf, Primary_Entity_Aspect);
      --  FIXME: check default port and generic association.
      Sem_Scopes.Close_Scope_Extension;
   end Sem_Configuration_Specification;

   function Sem_Create_Default_Binding_Indication
     (Comp : Iir_Component_Declaration;
      Entity_Unit : Iir_Design_Unit;
      Parent : Iir;
      Force : Boolean;
      Create_Map_Aspect : Boolean)
     return Iir_Binding_Indication
   is
      Entity : Iir_Entity_Declaration;
      Entity_Name : Iir;
      Aspect : Iir;
      Res : Iir;
      Design_Unit : Iir_Design_Unit;
   begin
      --  LRM 5.2.2
      --  The default binding indication consists of a default entity aspect,
      --  together with a default generic map aspect and a default port map
      --  aspect, as appropriate.

      if Entity_Unit = Null_Iir then
         if not Force then
            return Null_Iir;
         end if;

         --  LRM 5.2.2
         --  If no visible entity declaration has the same simple name as that
         --  of the instantiated component, then the default entity aspect is
         --  OPEN.
         Aspect := Create_Iir (Iir_Kind_Entity_Aspect_Open);
         Location_Copy (Aspect, Comp);
         Res := Create_Iir (Iir_Kind_Binding_Indication);
         Set_Entity_Aspect (Res, Aspect);
         return Res;
      else
         --  LRM 5.2.2
         --  Otherwise, the default entity aspect is of the form:
         --    ENTITY entity_name ( architecture_identifier)
         --  where the entity name is the simple name of the instantiated
         --  component and the architecture identifier is the same as the
         --  simple name of the most recently analyzed architecture body
         --  associated with the entity declaration.
         --
         --  If this rule is applied either to a binding indication contained
         --  within a configuration specification or to a component
         --  configuration that does not contain an explicit inner block
         --  configuration, then the architecture identifier is determined
         --  during elaboration of the design hierarchy containing the binding
         --  indication.
         --
         --  Likewise, if a component instantiation statement contains an
         --  instantiated unit containing the reserved word ENTITY, but does
         --  not contain an explicitly specified architecture identifier, this
         --  rule is applied during the elaboration of the design hierarchy
         --  containing a component instantiation statement.
         --
         --  In all other cases, this rule is applied during analysis of the
         --  binding indication.
         --
         --  It is an error if there is no architecture body associated with
         --  the entity declaration denoted by an entity name that is the
         --  simple name of the instantiated component.
         null;
      end if;

      Design_Unit := Libraries.Load_Primary_Unit
        (Get_Library (Get_Design_File (Entity_Unit)),
         Get_Identifier (Get_Library_Unit (Entity_Unit)),
         Parent);
      if Design_Unit = Null_Iir then
         --  Found an entity which is not in the library.
         raise Internal_Error;
      end if;

      Entity := Get_Library_Unit (Design_Unit);

      Res := Create_Iir (Iir_Kind_Binding_Indication);
      Aspect := Create_Iir (Iir_Kind_Entity_Aspect_Entity);
      Location_Copy (Aspect, Parent);

      Entity_Name := Create_Iir (Iir_Kind_Simple_Name);
      Location_Copy (Entity_Name, Parent);
      Set_Named_Entity (Entity_Name, Entity);

      Set_Entity_Name (Aspect, Entity_Name);
      Set_Entity_Aspect (Res, Aspect);

      if Create_Map_Aspect then
         --  LRM 5.2.2
         --  The default binding indication includes a default generic map
         --  aspect if the design entity implied by the entity aspect contains
         --  formal generics.
         Set_Generic_Map_Aspect_Chain
           (Res,
            Create_Default_Map_Aspect (Comp, Entity, Map_Generic, Parent));

         --  LRM 5.2.2
         --  The default binding indication includes a default port map aspect
         --  if the design entity implied by the entity aspect contains formal
         --  ports.
         Set_Port_Map_Aspect_Chain
           (Res, Create_Default_Map_Aspect (Comp, Entity, Map_Port, Parent));
      end if;

      return Res;
   end Sem_Create_Default_Binding_Indication;

   --  LRM 5.2.2
   --  The default binding indication includes a default generic map aspect
   --  if the design entity implied by the entity aspect contains formal
   --  generics.
   --
   --  The default generic map aspect associates each local generic in
   --  the corresponding component instantiation (if any) with a formal
   --  of the same simple name.
   --  It is an error if such a formal does not exist, or if its mode and
   --  type are not appropriate for such an association.
   --  Any remaining unassociated formals are associated with the actual
   --  designator OPEN.

   --  LRM 5.2.2
   --  The default binding indication includes a default port map aspect
   --  if the design entity implied by the entity aspect contains formal
   --  ports.
   --
   --  The default port map aspect associates each local port in the
   --  corresponding component instantiation (if any) with a formal of
   --  the same simple name.
   --  It is an error if such a formal does not exist, or if its mode
   --  and type are not appropriate for such an association.
   --  Any remaining unassociated formals are associated with the actual
   --  designator OPEN.
   function Create_Default_Map_Aspect
     (Comp : Iir; Entity : Iir; Kind : Map_Kind_Type; Parent : Iir)
     return Iir
   is
      Res, Last : Iir;
      Comp_El, Ent_El : Iir;
      Assoc : Iir;
      Name : Iir;
      Found : Natural;
      Comp_Chain : Iir;
      Ent_Chain : Iir;
      Error : Boolean;
   begin
      case Kind is
         when Map_Generic =>
            Ent_Chain := Get_Generic_Chain (Entity);
            Comp_Chain := Get_Generic_Chain (Comp);
         when Map_Port =>
            Ent_Chain := Get_Port_Chain (Entity);
            Comp_Chain := Get_Port_Chain (Comp);
      end case;

      --  If no formal, then there is no association list.
      if Ent_Chain = Null_Iir then
         return Null_Iir;
      end if;

      --  No error found yet.
      Error := False;

      Sub_Chain_Init (Res, Last);
      Found := 0;
      Ent_El := Ent_Chain;
      while Ent_El /= Null_Iir loop
         --  Find the component generic/port with the same name.
         Comp_El := Find_Name_In_Chain (Comp_Chain, Get_Identifier (Ent_El));
         if Comp_El = Null_Iir then
            Assoc := Create_Iir (Iir_Kind_Association_Element_Open);
            Location_Copy (Assoc, Parent);
         else
            if Are_Nodes_Compatible (Comp_El, Ent_El) = Not_Compatible then
               if not Error then
                  Error_Msg_Sem
                    (+Parent, "for default port binding of %n:",
                     (1 => +Parent), Cont => True);
               end if;
               Error_Msg_Sem
                 (+Parent, "type of %n declarared at %l",
                  (+Comp_El, +Comp_El), Cont => True);
               Error_Msg_Sem
                 (+Parent, "not compatible with type of %n declarared at %l",
                  (+Ent_El, +Ent_El));
               Error := True;
            elsif Kind = Map_Port
              and then not Check_Port_Association_Mode_Restrictions
              (Ent_El, Comp_El, Null_Iir)
            then
               if not Error then
                  Error_Msg_Sem (+Parent, "for default port binding of %n",
                                 (1 => +Parent), Cont => True);
               end if;
               Error_Msg_Sem (+Parent, "cannot associate "
                                & Get_Mode_Name (Get_Mode (Ent_El))
                                & " %n declarared at %l",
                              (+Ent_El, +Ent_El), Cont => True);
               Error_Msg_Sem (+Parent, "with actual port of mode "
                                & Get_Mode_Name (Get_Mode (Comp_El))
                                & " declared at %l", +Comp_El);
               Error := True;
            end if;
            Assoc := Create_Iir (Iir_Kind_Association_Element_By_Expression);
            Location_Copy (Assoc, Parent);
            Name := Build_Simple_Name (Comp_El, Parent);
            Set_Type (Name, Get_Type (Comp_El));
            Set_Actual (Assoc, Name);
            if Kind = Map_Port then
               Check_Port_Association_Bounds_Restrictions
                 (Ent_El, Comp_El, Assoc);
            end if;
            Found := Found + 1;
         end if;
         Set_Whole_Association_Flag (Assoc, True);
         Name := Build_Simple_Name (Ent_El, Parent);
         Set_Type (Name, Get_Type (Ent_El));
         Set_Formal (Assoc, Name);
         if Kind = Map_Port
           and then not Error
           and then Comp_El /= Null_Iir
         then
            Set_Collapse_Signal_Flag
              (Assoc, Can_Collapse_Signals (Assoc, Ent_El));
         end if;
         Sub_Chain_Append (Res, Last, Assoc);
         Ent_El := Get_Chain (Ent_El);
      end loop;
      if Iir_Chains.Get_Chain_Length (Comp_Chain) /= Found then
         --  At least one component generic/port cannot be associated with
         --  the entity one.
         Error := True;
         --  Disp unassociated interfaces.
         Comp_El := Comp_Chain;
         while Comp_El /= Null_Iir loop
            Ent_El := Find_Name_In_Chain (Ent_Chain, Get_Identifier (Comp_El));
            if Ent_El = Null_Iir then
               Error_Msg_Sem (+Parent, "%n has no association in %n",
                              (+Comp_El, +Entity));
            end if;
            Comp_El := Get_Chain (Comp_El);
         end loop;
      end if;
      if Error then
         return Null_Iir;
      else
         return Res;
      end if;
   end Create_Default_Map_Aspect;

   --  LRM93 �5.2.2
   function Get_Visible_Entity_Declaration (Comp: Iir_Component_Declaration)
     return Iir_Design_Unit
   is
      --  Return the design_unit if DECL is an entity declaration or the
      --  design unit of an entity declaration.  Otherwise return Null_Iir.
      --  This double check is needed as the interpretation may be both.
      function Is_Entity_Declaration (Decl : Iir) return Iir is
      begin
         if Get_Kind (Decl) = Iir_Kind_Entity_Declaration then
            return Get_Design_Unit (Decl);
         elsif Get_Kind (Decl) = Iir_Kind_Design_Unit
           and then
           Get_Kind (Get_Library_Unit (Decl)) = Iir_Kind_Entity_Declaration
         then
            return Decl;
         else
            return Null_Iir;
         end if;
      end Is_Entity_Declaration;

      Name : constant Name_Id := Get_Identifier (Comp);
      Inter : Name_Interpretation_Type;
      Decl : Iir;
      Res : Iir;
      Target_Lib : Iir;
   begin
      Inter := Get_Interpretation (Name);

      if Valid_Interpretation (Inter) then
         --  LRM93 5.2.2 Default binding indication
         --  A visible entity declaration is either:
         --
         --  a) An entity declaration that has the same simple name as that of
         --     the instantiated component and that is directly visible
         --     (see 10.3),
         Decl := Get_Declaration (Inter);
         Res := Is_Entity_Declaration (Decl);
         if Res /= Null_Iir then
            return Res;
         end if;

         --  b)  An entity declaration that has the same simple name that of
         --      the instantiated component and that would be directly
         --      visible in the absence of a directly visible (see 10.3)
         --      component declaration with the same simple name as that
         --      of the entity declaration, or
         if Get_Kind (Decl) = Iir_Kind_Component_Declaration then
            Inter := Get_Under_Interpretation (Name);
            if Valid_Interpretation (Inter) then
               Decl := Get_Declaration (Inter);
               Res := Is_Entity_Declaration (Decl);
               if Res /= Null_Iir then
                  return Res;
               end if;
            end if;
         end if;
      end if;

      --  VHDL02:
      --  c) An entity declaration denoted by "L.C", where L is the target
      --     library and C is the simple name of the instantiated component.
      --     The target library is the library logical name of the library
      --     containing the design unit in which the component C is
      --     declared.
      if Flags.Flag_Syn_Binding
        or Flags.Vhdl_Std >= Vhdl_02
        or Flags.Vhdl_Std = Vhdl_93c
      then
         --  Find target library.
         Target_Lib := Comp;
         while Get_Kind (Target_Lib) /= Iir_Kind_Library_Declaration loop
            Target_Lib := Get_Parent (Target_Lib);
         end loop;

         Decl := Libraries.Find_Primary_Unit (Target_Lib, Name);
         if Decl /= Null_Iir then
            Res := Is_Entity_Declaration (Decl);
            if Res /= Null_Iir then
               return Res;
            end if;
         end if;
      end if;

      --  --syn-binding
      --  Search for any entity.
      if Flags.Flag_Syn_Binding then
         Decl := Libraries.Find_Entity_For_Component (Name);
         if Decl /= Null_Iir then
            return Decl;
         end if;
      end if;

      return Null_Iir;
   end Get_Visible_Entity_Declaration;

   --  Explain why there is no default binding for COMP.
   procedure Explain_No_Visible_Entity (Comp: Iir_Component_Declaration)
   is
      Inter : Name_Interpretation_Type;
      Name : Name_Id;
      Decl : Iir;
   begin
      Name := Get_Identifier (Comp);
      Inter := Get_Interpretation (Name);

      if Valid_Interpretation (Inter) then
         --  LRM93 5.2.2 Default binding indication
         --  A visible entity declaration is either:
         --
         --  a) An entity declaration that has the same simple name as that of
         --     the instantiated component and that is directly visible
         --     (see 10.3),
         Decl := Get_Declaration (Inter);
         Warning_Msg_Elab
           (Warnid_Default_Binding, Decl, "visible declaration for %i", +Name);

         --  b)  An entity declaration that has the same simple name that of
         --      the instantiated component and that would be directly
         --      visible in the absence of a directly visible (see 10.3)
         --      component declaration with the same simple name as that
         --      of the entity declaration, or
         if Get_Kind (Decl) = Iir_Kind_Component_Declaration then
            Inter := Get_Under_Interpretation (Name);
            if Valid_Interpretation (Inter) then
               Decl := Get_Declaration (Inter);
               Warning_Msg_Elab (Warnid_Default_Binding, Comp,
                                 "interpretation behind the component is %n",
                                 +Decl);
            end if;
         end if;
      end if;

      --  VHDL02:
      --  c) An entity declaration denoted by "L.C", where L is the target
      --     library and C is the simple name of the instantiated component.
      --     The target library is the library logical name of the library
      --     containing the design unit in which the component C is
      --     declared.
      if Flags.Vhdl_Std >= Vhdl_02
        or else Flags.Vhdl_Std = Vhdl_93c
      then
         Decl := Comp;
         while Get_Kind (Decl) /= Iir_Kind_Library_Declaration loop
            Decl := Get_Parent (Decl);
         end loop;

         Warning_Msg_Elab (Warnid_Default_Binding, Comp,
                           "no entity %i in %n", (+Name, +Decl));
      end if;
   end Explain_No_Visible_Entity;

   procedure Sem_Specification_Chain (Decls_Parent : Iir; Parent_Stmts: Iir)
   is
      Decl: Iir;
   begin
      Decl := Get_Declaration_Chain (Decls_Parent);
      while Decl /= Null_Iir loop
         case Get_Kind (Decl) is
            when Iir_Kind_Configuration_Specification =>
               Sem_Configuration_Specification (Parent_Stmts, Decl);
            when others =>
               null;
         end case;
         Decl := Get_Chain (Decl);
      end loop;
   end Sem_Specification_Chain;
end Sem_Specs;
