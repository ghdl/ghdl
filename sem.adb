--  Semantic analysis pass.
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
with Errorout; use Errorout;
with Std_Package; use Std_Package;
with Libraries;
with Std_Names;
with Sem_Scopes; use Sem_Scopes;
with Sem_Expr; use Sem_Expr;
with Sem_Names; use Sem_Names;
with Sem_Specs; use Sem_Specs;
with Sem_Decls; use Sem_Decls;
with Sem_Assocs; use Sem_Assocs;
with Iirs_Utils; use Iirs_Utils;
with Flags; use Flags;
with Name_Table;
with Str_Table;
with Sem_Stmts; use Sem_Stmts;
with Sem_Types; use Sem_Types;
with Iir_Chains;
with Xrefs; use Xrefs;

package body Sem is
   -- Forward declarations.
   procedure Sem_Context_Clauses (Design_Unit: Iir_Design_Unit);
   procedure Sem_Block_Configuration
     (Block_Conf : Iir_Block_Configuration; Father: Iir);
   procedure Sem_Component_Configuration
     (Conf : Iir_Component_Configuration; Father : Iir);

   procedure Add_Dependence (Unit : Iir) is
   begin
      Add_Dependence (Get_Current_Design_Unit, Unit);
   end Add_Dependence;

   --  LRM 1.1  Entity declaration.
   procedure Sem_Entity_Declaration (Entity: Iir_Entity_Declaration)
   is
      Unit : Iir_Design_Unit;
      Implicit : Implicit_Signal_Declaration_Type;
   begin
      Unit := Get_Design_Unit (Entity);
      Xrefs.Xref_Decl (Entity);
      Sem_Scopes.Add_Name (Unit);
      Set_Visible_Flag (Unit, True);

      Set_Is_Within_Flag (Entity, True);

      --  LRM 10.1
      --  1.  An entity declaration, together with a corresponding architecture
      --      body.
      Open_Declarative_Region;

      -- Sem generics.
      Sem_Interface_Chain (Get_Generic_Chain (Entity), Interface_Generic);

      -- Sem ports.
      Sem_Interface_Chain (Get_Port_Chain (Entity), Interface_Port);

      -- entity declarative part.
      Push_Signals_Declarative_Part (Implicit, Entity);
      Sem_Declaration_Chain (Entity, not Flags.Flag_Whole_Analyze);
      Sem_Specification_Chain (Entity, Null_Iir);

      --  Check for missing subprogram bodies.
      Check_Full_Declaration (Entity, Entity);

      -- statements.
      Sem_Concurrent_Statement_Chain (Entity, True);
      Pop_Signals_Declarative_Part (Implicit);
      Close_Declarative_Region;
      Set_Is_Within_Flag (Entity, False);
   end Sem_Entity_Declaration;

   --  Get the entity unit for LIBRARY_UNIT (an architecture or a
   --  configuration declaration).
   --  Return NULL_IIR in case of error (not found, bad library).
   function Sem_Entity_Name (Library_Unit : Iir) return Iir
   is
      Name : Iir;
      Library : Iir_Library_Declaration;
      Entity_Unit : Iir;
      Entity_Library : Iir;
   begin
      Name := Get_Entity (Library_Unit);
      Library := Get_Library
        (Get_Design_File (Get_Design_Unit (Library_Unit)));
      if Get_Kind (Name) = Iir_Kind_Simple_Name then
         Entity_Unit := Libraries.Load_Primary_Unit
           (Library, Get_Identifier (Name), Library_Unit);
         if Entity_Unit = Null_Iir then
            Error_Msg_Sem ("entity " & Disp_Node (Name) & " was not analysed",
                           Library_Unit);
            return Null_Iir;
         end if;
         Set_Named_Entity (Name, Entity_Unit);
      else
         Sem_Name (Name, False);
         Entity_Unit := Get_Named_Entity (Name);
         if Entity_Unit = Error_Mark then
            return Null_Iir;
         end if;
      end if;
      if Get_Kind (Entity_Unit) = Iir_Kind_Design_Unit then
         Entity_Library := Get_Library_Unit (Entity_Unit);
         Xrefs.Xref_Ref (Name, Entity_Library);
         if Get_Kind (Entity_Library) = Iir_Kind_Entity_Declaration then
            --  LRM 1.2 Architecture bodies
            --  For a given design entity, both the entity declaration and the
            --  associated architecture body must reside in the same library.

            --  LRM 1.3 Configuration Declarations
            --  For a configuration of a given design entity, both the
            --  configuration declaration and the corresponding entity
            --  declaration must reside in the same library.
            if Get_Library (Get_Design_File (Entity_Unit)) /= Library then
               Error_Msg_Sem
                 (Disp_Node (Entity_Library) & " does not reside in "
                  & Disp_Node (Library), Library_Unit);
               return Null_Iir;
            end if;
            return Entity_Unit;
         end if;
      end if;

      Error_Msg_Sem ("entity name expected, found " & Disp_Node (Entity_Unit),
                     Library_Unit);
      return Null_Iir;
   end Sem_Entity_Name;

   --  LRM 1.2  Architecture bodies.
   procedure Sem_Architecture_Declaration (Arch: Iir_Architecture_Declaration)
   is
      Unit : Iir_Design_Unit;
      Entity_Unit : Iir_Design_Unit;
      Entity_Library : Iir_Entity_Declaration;
   begin
      Xrefs.Xref_Decl (Arch);
      -- First, find the entity.
      Entity_Unit := Sem_Entity_Name (Arch);
      if Entity_Unit = Null_Iir then
         return;
      end if;
      Entity_Library := Get_Library_Unit (Entity_Unit);

      --  LRM93 11.4
      --   In each case, the second unit depends on the first unit.
      --  GHDL: an architecture depends on its entity.
      Add_Dependence (Entity_Unit);

      -- Transforms an identifier into an entity_decl.
      Set_Entity (Arch, Entity_Library);

      Add_Context_Clauses (Entity_Unit);

      Set_Is_Within_Flag (Arch, True);
      Set_Is_Within_Flag (Entity_Library, True);

      --  Makes the entity name visible.
      --  FIXME: quote LRM.
      Sem_Scopes.Add_Name (Entity_Unit, Get_Identifier (Entity_Unit), False);

      --  LRM 10.1 Declarative Region
      --  1. An entity declaration, together with a corresponding architecture
      --     body.
      Open_Declarative_Region;
      Sem_Scopes.Add_Entity_Declarations (Entity_Library);

      --  LRM02 1.2  Architecture bodies
      --  For the purpose of interpreting the scope and visibility of the
      --  identifier (see 10.2 and 10.3), the declaration of the identifier is
      --  considered to occur after the final declarative item of the entity
      --  declarative part of the corresponding entity declaration.
      --
      --  FIXME: before VHDL-02, an architecture is not a declaration.
      Unit := Get_Design_Unit (Arch);
      Sem_Scopes.Add_Name (Unit, Get_Identifier (Unit), True);
      Set_Visible_Flag (Unit, True);

      --  LRM02 10.1  Declarative region
      --  The declarative region associated with an architecture body is
      --  considered to occur immediatly within the declarative region
      --  associated with the entity declaration corresponding to the given
      --  architecture body.
      if Vhdl_Std >= Vhdl_02 then
         Open_Declarative_Region;
      end if;
      Sem_Block (Arch, True);
      if Vhdl_Std >= Vhdl_02 then
         Close_Declarative_Region;
      end if;

      Close_Declarative_Region;
      Set_Is_Within_Flag (Arch, False);
      Set_Is_Within_Flag (Entity_Library, False);
   end Sem_Architecture_Declaration;

   --  Return the real resolver used for (sub) object OBJ.
   --  Return NULL_IIR if none.
   function Get_Resolver (Obj : Iir) return Iir
   is
      Obj_Type : Iir;
      Res : Iir;
   begin
      case Get_Kind (Obj) is
         when Iir_Kind_Indexed_Name
           | Iir_Kind_Slice_Name
           | Iir_Kind_Selected_Element =>
            Res := Get_Resolver (Get_Prefix (Obj));
            if Res /= Null_Iir then
               return Res;
            end if;
         when Iir_Kind_Signal_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_Guard_Signal_Declaration =>
            null;
         when Iir_Kind_Object_Alias_Declaration =>
            return Get_Resolver (Get_Name (Obj));
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            return Get_Resolver (Get_Named_Entity (Obj));
         when others =>
            Error_Kind ("get_resolved", Obj);
      end case;

      Obj_Type := Get_Type (Obj);
      if Get_Kind (Obj_Type) in Iir_Kinds_Subtype_Definition then
         return Get_Resolution_Function (Obj_Type);
      else
         return Null_Iir;
      end if;
   end Get_Resolver;

   --  Return TRUE iff the actual of ASSOC can be the formal.
   --  ASSOC must be an association_element_by_expression.
   function Can_Collapse_Signals (Assoc : Iir; Formal : Iir) return Boolean
   is
      Actual : Iir;
      Actual_Res : Iir;
      Formal_Res : Iir;
      Formal_Base : Iir;
      Actual_Base : Iir;
   begin
      --  If there is a conversion, signals types are not necessarily
      --  the same, and sharing is not possible.
      --  FIXME: optimize type conversions
      --    (unsigned <-> signed <-> std_ulogic_vector <-> ...)
      if Get_In_Conversion (Assoc) /= Null_Iir
        or else Get_Out_Conversion (Assoc) /= Null_Iir
      then
         return False;
      end if;

      --  Here we may assume formal and actual have the same type and the
      --  same lengths.  This is caught at elaboration time.

      Actual := Name_To_Object (Get_Actual (Assoc));
      if Actual = Null_Iir then
         --  This is an expression.
         return False;
      end if;

      Formal_Base := Get_Base_Name (Formal);
      Actual_Base := Get_Object_Prefix (Actual);

      --  If the formal is of mode IN, then it has no driving value, and its
      --  effective value is the effective value of the actual.
      --  Always collapse in this case.
      if Get_Mode (Formal_Base) = Iir_In_Mode then
         return True;
      end if;

      --  Otherwise, these rules are applied:
      --
      --  In this table, E means element, S means signal.
      --                 Er means the element is resolved,
      --                 Sr means the signal is resolved (at the signal level).
      --
      --                            Actual
      --               | E,S   | Er,S  | E,Sr  | Er,Sr |
      --         ------+-------+-------+-------+-------+
      --         E,S   |collap | no(3) | no(3) | no(3) |
      --         ------+-------+-------+-------+-------+
      --         Er,S  | no(1) |if same| no(2) | no(2) |
      --  Formal ------+-------+-------+-------+-------+
      --         E,Sr  | no(1) | no(2) |if same| no(4) |
      --         ------+-------+-------+-------+-------+
      --         Er,Sr | no(1) | no(2) | no(4) |if same|
      --         ------+-------+-------+-------+-------+
      --
      --  Notes: (1): formal may have several sources.
      --         (2): resolver is not the same.
      --         (3): this prevents to catch several sources error in instance.
      --         (4): resolver is not the same, because the types are not the
      --              same.
      --
      --  Furthermore, signals cannot be collapsed if the kind (none, bus or
      --  register) is not the same.
      --
      --  Default value:  default value is the effective value.

      --  Resolution function.
      Actual_Res := Get_Resolver (Actual);
      Formal_Res := Get_Resolver (Formal);

      --  If the resolutions are not the same, signals cannot be collapsed.
      if Actual_Res /= Formal_Res then
         return False;
      end if;

      --  If neither the actual nor the formal is resolved, then collapsing is
      --  possible.
      --  (this is case ES/ES).
      if Actual_Res = Null_Iir and Formal_Res = Null_Iir then
         return True;
      end if;

      --  If the formal can have sources and is guarded, but the actual is
      --  not guarded (or has not the same kind of guard), signals cannot
      --  be collapsed.
      if Get_Signal_Kind (Formal_Base) /= Get_Signal_Kind (Actual_Base) then
         return False;
      end if;

      return True;
   end Can_Collapse_Signals;

   --  INTER_PARENT contains generics and ports interfaces;
   --  ASSOC_PARENT constains generics and ports map aspects.
   procedure Sem_Generic_Port_Association_Chain
     (Inter_Parent : Iir; Assoc_Parent : Iir)
   is
      El : Iir;
      Actual : Iir;
      Prefix : Iir;
      Object : Iir;
      Match : Boolean;
      Assoc_Chain : Iir;
      Miss_Generic : Missing_Type;
      Miss_Port : Missing_Type;
      Inter : Iir;
      Formal : Iir;
   begin
      --  Note: CHECK_MATCH argument of sem_subprogram_arguments must be
      --   true if parent is a component instantiation.
      case Get_Kind (Assoc_Parent) is
         when Iir_Kind_Component_Instantiation_Statement =>
            --  LRM 9.6 Component Instantiation Statement
            --  Each local generic (or subelement or slice thereof) must be
            --  associated {VHDL87: exactly}{VHDL93: at most} once.
            --  ...
            --  Each local port (or subelement or slice therof) must be
            --  associated {VHDL87: exactly}{VHDL93: at most} once.
            if Flags.Vhdl_Std = Vhdl_87 then
               Miss_Generic := Missing_Generic;
               Miss_Port := Missing_Port;
            else
               Miss_Generic := Missing_Allowed;
               if Get_Kind (Inter_Parent) = Iir_Kind_Entity_Declaration then
                  --  FIXME: to be checked.
                  --  Ghdl: for a direct instantiation, follow rules of
                  --  LRM 1.1.1.2 Ports.
                  Miss_Port := Missing_Port;
               else
                  Miss_Port := Missing_Allowed;
               end if;
            end if;
         when Iir_Kind_Binding_Indication =>
            --  LRM 5.2.1.2  Generic map and port map aspects
            Miss_Generic := Missing_Allowed;
            Miss_Port := Missing_Allowed;
         when Iir_Kind_Block_Header =>
            --  FIXME: it is possible to have port unassociated ?
            Miss_Generic := Missing_Generic;
            Miss_Port := Missing_Port;
         when others =>
            Error_Kind ("sem_generic_port_association_list", Assoc_Parent);
      end case;

      Assoc_Chain := Get_Generic_Map_Aspect_Chain (Assoc_Parent);
      if Sem_Actual_Of_Association_Chain (Assoc_Chain) then
         Sem_Association_Chain
           (Get_Generic_Chain (Inter_Parent), Assoc_Chain,
            True, Miss_Generic, Assoc_Parent, Match);
         Set_Generic_Map_Aspect_Chain (Assoc_Parent, Assoc_Chain);

         --  LRM 5.2.1.2   Generic map and port map aspects
         --  An actual associated with a formal generic map aspect must be an
         --  expression or the reserved word open;
         if Match then
            El := Assoc_Chain;
            while El /= Null_Iir loop
               case Get_Kind (El) is
                  when Iir_Kind_Association_Element_By_Expression =>
                     Check_Read (Get_Actual (El));
                  when Iir_Kind_Association_Element_Open =>
                     null;
                  when Iir_Kind_Association_Element_By_Individual =>
                     null;
                  when others =>
                     Error_Kind
                       ("sem_generic_port_map_association_chain(1)", El);
               end case;
               El := Get_Chain (El);
            end loop;
         end if;
      end if;

      Assoc_Chain := Get_Port_Map_Aspect_Chain (Assoc_Parent);
      if not Sem_Actual_Of_Association_Chain (Assoc_Chain) then
         return;
      end if;
      Sem_Association_Chain (Get_Port_Chain (Inter_Parent), Assoc_Chain,
                             True, Miss_Port, Assoc_Parent, Match);
      Set_Port_Map_Aspect_Chain (Assoc_Parent, Assoc_Chain);
      if not Match then
         return;
      end if;

      --  LRM 5.2.1.2  Generic map and port map aspects
      --  [...]; an actual associated with a formal port in a port map aspect
      --  must be a signal, an expression, or the reserved word open.
      --
      --  Certain restriction apply to the actual associated with a formal in
      --  a port map aspect; these restrictions are described in 1.1.1.2

      --  LRM93 1.1.1.2
      --  The actual, if a port or signal, must be denoted by a static name.
      --  The actual, if an expression, must be a globally static expression.
      El := Assoc_Chain;
      Inter := Get_Port_Chain (Inter_Parent);
      while El /= Null_Iir loop
         Formal := Get_Formal (El);
         if Formal = Null_Iir then
            Formal := Inter;
            Inter := Get_Chain (Inter);
         else
            Inter := Null_Iir;
         end if;
         if Get_Kind (El) = Iir_Kind_Association_Element_By_Expression then
            Actual := Get_Actual (El);
            --  There has been an error, exit from the loop.
            exit when Actual = Null_Iir;
            Object := Name_To_Object (Actual);
            if Object = Null_Iir then
               Prefix := Actual;
            else
               Prefix := Get_Object_Prefix (Object);
            end if;
            case Get_Kind (Prefix) is
               when Iir_Kind_Signal_Declaration
                 | Iir_Kind_Signal_Interface_Declaration
                 | Iir_Kind_Guard_Signal_Declaration
                 | Iir_Kinds_Signal_Attribute =>
                  --  Port or signal.
                  Set_Collapse_Signal_Flag
                    (El, Can_Collapse_Signals (El, Formal));
                  if Get_Name_Staticness (Object) < Globally then
                     Error_Msg_Sem ("actual must be a static name", Actual);
                  end if;
                  if Get_Kind (Prefix)
                    = Iir_Kind_Signal_Interface_Declaration
                  then
                     declare
                        P : Boolean;
                        pragma Unreferenced (P);
                     begin
                        P := Check_Port_Association_Restriction
                          (Get_Base_Name (Formal), Prefix, El);
                     end;
                  end if;
               when others =>
                  --  Expression.
                  Set_Collapse_Signal_Flag (El, False);

                  --  If there is an IN conversion, re-integrate it into
                  --  the actual.
                  declare
                     In_Conv : Iir;
                  begin
                     In_Conv := Get_In_Conversion (El);
                     if In_Conv /= Null_Iir then
                        Set_In_Conversion (El, Null_Iir);
                        Set_Expr_Staticness
                          (In_Conv, Get_Expr_Staticness (Actual));
                        Actual := In_Conv;
                        Set_Actual (El, Actual);
                     end if;
                  end;
                  if Flags.Vhdl_Std >= Vhdl_93c then
                     --  LRM93 1.1.1.2 Ports
                     --  Moreover, the ports of a block may be associated
                     --  with an expression, in order to provide these ports
                     --  with constant driving values; such ports must be
                     --  of mode in.
                     if Get_Mode (Get_Base_Name (Formal)) /= Iir_In_Mode
                     then
                        Error_Msg_Sem ("only 'in' ports may be associated "
                                       & "with expression", El);
                     end if;

                     --  LRM93 1.1.1.2 Ports
                     --  The actual, if an expression, must be a globally
                     --  static expression.
                     if Get_Expr_Staticness (Actual) < Globally then
                        Error_Msg_Sem
                          ("actual expression must be globally static",
                           Actual);
                     end if;
                  else
                     Error_Msg_Sem
                       ("cannot associate ports with expression in vhdl87",
                        El);
                  end if;
            end case;
         end if;
         El := Get_Chain (El);
      end loop;
   end Sem_Generic_Port_Association_Chain;

   --  LRM 1.3  Configuration Declarations.
   procedure Sem_Configuration_Declaration (Decl: Iir)
   is
      Unit : Iir_Design_Unit;
      Entity_Design: Iir_Design_Unit;
   begin
      Xref_Decl (Decl);

      --  LRM 1.3
      --  The entity name identifies the name of the entity declaration that
      --  defines the design entity at the apex of the design hierarchy.
      Entity_Design := Sem_Entity_Name (Decl);
      if Entity_Design = Null_Iir then
         return;
      end if;
      Set_Entity (Decl, Entity_Design);

      --  LRM 11.4
      --  A primary unit whose name is referenced within a given design unit
      --  must be analyzed prior to the analysis of the given design unit.
      Add_Dependence (Entity_Design);

      Unit := Get_Design_Unit (Decl);
      Sem_Scopes.Add_Name (Unit);
      Set_Visible_Flag (Unit, True);

      --  LRM 10.1 Declarative Region
      --  2.  A configuration declaration.
      Open_Declarative_Region;

      --  LRM93 10.2
      --  In addition to the above rules, the scope of any declaration that
      --  includes the end of the declarative part of a given block (wether
      --  it be an external block defined by a design entity or an internal
      --  block defined by a block statement) extends into a configuration
      --  declaration that configures the given block.
      Add_Context_Clauses (Entity_Design);
      Sem_Scopes.Add_Entity_Declarations (Get_Library_Unit (Entity_Design));

      Sem_Declaration_Chain (Decl, False);
      --  GHDL: no need to check for missing subprogram bodies, since they are
      --  not allowed in configuration declarations.

      Sem_Block_Configuration (Get_Block_Configuration (Decl), Decl);
      Close_Declarative_Region;
   end Sem_Configuration_Declaration;

   --  LRM 1.3.1  Block Configuration.
   --  FATHER is the block_configuration, configuration_declaration,
   --  component_configuration containing the block_configuration BLOCK_CONF.
   procedure Sem_Block_Configuration
     (Block_Conf : Iir_Block_Configuration; Father: Iir)
   is
      El : Iir;
      Block : Iir;
   begin
      case Get_Kind (Father) is
         when Iir_Kind_Configuration_Declaration =>
            --  LRM93 1.3.1
            --  If a block configuration appears immediately within a
            --  configuration declaration, then the block specification of that
            --  block configuration must be an architecture name, and that
            --  architecture name must denote a design entity body whose
            --  interface is defined by the entity declaration denoted by the
            --  entity name of the enclosing configuration declaration.
            declare
               Block_Spec : Iir;
               Arch : Iir_Architecture_Declaration;
               Design: Iir_Design_Unit;
            begin
               Block_Spec := Get_Block_Specification (Block_Conf);
               --  FIXME: handle selected name.
               if Get_Kind (Block_Spec) /= Iir_Kind_Simple_Name then
                  Error_Msg_Sem ("architecture name expected", Block_Spec);
                  return;
               end if;
               --  LRM 10.3 rule b)
               --  For an architecture body associated with a given entity
               --  declaration: at the place of the block specification in a
               --  block configuration for an external block whose interface
               --  is defined by that entity declaration.
               Design := Libraries.Load_Secondary_Unit
                 (Get_Entity (Father), Get_Identifier (Block_Spec),
                  Block_Conf);
               if Design = Null_Iir then
                  Error_Msg_Sem
                    ("no architecture '" & Image_Identifier (Block_Spec) & "'",
                     Block_Conf);
                  return;
               end if;
               Arch := Get_Library_Unit (Design);
               Xref_Ref (Block_Spec, Arch);
               Free_Iir (Block_Spec);
               Set_Block_Specification (Block_Conf, Arch);
               Block := Arch;
               Add_Dependence (Design);
            end;

         when Iir_Kind_Component_Configuration =>
            --  LRM93 1.3.1
            --  If a block configuration appears immediately within a component
            --  configuration, then the corresponding components must be
            --  fully bound, the block specification of that block
            --  configuration must be an architecture name, and that
            --  architecture name must denote the same architecture body as
            --  that to which the corresponding components are bound.
            declare
               Block_Spec : Iir;
               Arch : Iir_Architecture_Declaration;
               Design: Iir_Design_Unit;
               Entity_Aspect : Iir;
               Comp_Arch : Iir;
            begin
               Entity_Aspect :=
                 Get_Entity_Aspect (Get_Binding_Indication (Father));
               if Entity_Aspect = Null_Iir or else
                 Get_Kind (Entity_Aspect) /= Iir_Kind_Entity_Aspect_Entity
               then
                  Error_Msg_Sem ("corresponding component not fully bound",
                                 Block_Conf);
               end if;

               Block_Spec := Get_Block_Specification (Block_Conf);
               --  FIXME: handle selected name.
               if Get_Kind (Block_Spec) /= Iir_Kind_Simple_Name then
                  Error_Msg_Sem ("architecture name expected", Block_Spec);
                  return;
               end if;

               Comp_Arch := Get_Architecture (Entity_Aspect);
               if Comp_Arch /= Null_Iir then
                  if Get_Kind (Comp_Arch) /= Iir_Kind_Simple_Name then
                     raise Internal_Error;
                  end if;
                  if Get_Identifier (Comp_Arch) /= Get_Identifier (Block_Spec)
                  then
                     Error_Msg_Sem
                       ("block specification name is different from "
                        & "component architecture name", Block_Spec);
                     return;
                  end if;
               end if;

               Design := Libraries.Load_Secondary_Unit
                 (Get_Entity (Entity_Aspect), Get_Identifier (Block_Spec),
                  Block_Conf);
               if Design = Null_Iir then
                  Error_Msg_Sem
                    ("no architecture '" & Image_Identifier (Block_Spec) & "'",
                     Block_Conf);
                  return;
               end if;
               Arch := Get_Library_Unit (Design);
               Xref_Ref (Block_Spec, Arch);
               Free_Iir (Block_Spec);
               Set_Block_Specification (Block_Conf, Arch);
               Block := Arch;
            end;

         when Iir_Kind_Block_Configuration =>
            --  LRM93 1.3.1
            --  If a block configuration appears immediately within another
            --  block configuration, then the block specification of the
            --  contained block configuration must be a block statement or
            --  generate statement label, and the label must denote a block
            --  statement or generate statement that is contained immediatly
            --  within the block denoted by the block specification of the
            --  containing block configuration.
            declare
               Block_Spec : Iir;
               Block_Stmts : Iir;
               Block_Spec_Kind : Iir_Kind;
               Prev : Iir_Block_Configuration;
            begin
               Block_Spec := Get_Block_Specification (Block_Conf);
               --  Remember the kind of BLOCK_SPEC, since the node can be free
               --  by find_declaration if it is a simple name.
               Block_Spec_Kind := Get_Kind (Block_Spec);
               case Block_Spec_Kind is
                  when Iir_Kind_Simple_Name =>
                     Block := Block_Spec;
                  when Iir_Kind_Parenthesis_Name =>
                     Block := Get_Prefix (Block_Spec);
                  when Iir_Kind_Slice_Name =>
                     Block := Get_Prefix (Block_Spec);
                  when others =>
                     Error_Msg_Sem ("label expected", Block_Spec);
                     return;
               end case;
               Block := Find_Declaration (Block, Decl_Label);
               if Block = Null_Iir then
                  return;
               end if;
               case Get_Kind (Block) is
                  when Iir_Kind_Block_Statement =>
                     if Block_Spec_Kind /= Iir_Kind_Simple_Name then
                        Error_Msg_Sem
                          ("label does not denote a generate statement",
                           Block_Spec);
                     end if;
                     Prev := Get_Block_Block_Configuration (Block);
                     if Prev /= Null_Iir then
                        Error_Msg_Sem
                          (Disp_Node (Block) & " was already configured at "
                           & Disp_Location (Prev),
                           Block_Conf);
                        return;
                     end if;
                     Set_Block_Block_Configuration (Block, Block_Conf);
                  when Iir_Kind_Generate_Statement =>
                     if Block_Spec_Kind /= Iir_Kind_Simple_Name
                       and then Get_Kind (Get_Generation_Scheme (Block))
                       /= Iir_Kind_Iterator_Declaration
                     then
                        --  LRM93 1.3
                        --  If the block specification of a block configuration
                        --  contains a generate statement label, and if this
                        --  label contains an index specification, then it is
                        --  an error if the generate statement denoted by the
                        --  label does not have a generation scheme including
                        --  the reserved word for.
                        Error_Msg_Sem ("generate statement does not has a for",
                                       Block_Spec);
                        return;
                     end if;
                     Set_Prev_Block_Configuration
                       (Block_Conf, Get_Generate_Block_Configuration (Block));
                     Set_Generate_Block_Configuration (Block, Block_Conf);
                  when others =>
                     Error_Msg_Sem ("block statement label expected",
                                    Block_Conf);
                     return;
               end case;
               Block_Stmts := Get_Concurrent_Statement_Chain
                 (Get_Block_From_Block_Specification
                  (Get_Block_Specification (Father)));
               if not Is_In_Chain (Block_Stmts, Block) then
                  Error_Msg_Sem
                    ("label does not denotes an inner block statement",
                     Block_Conf);
                  return;
               end if;

               if Block_Spec_Kind = Iir_Kind_Parenthesis_Name then
                  Block_Spec := Sem_Index_Specification
                    (Block_Spec, Get_Type (Get_Generation_Scheme (Block)));
                  if Block_Spec /= Null_Iir then
                     Set_Prefix (Block_Spec, Block);
                     Set_Block_Specification (Block_Conf, Block_Spec);
                     Block_Spec_Kind := Get_Kind (Block_Spec);
                  end if;
               end if;

               case Block_Spec_Kind is
                  when Iir_Kind_Simple_Name =>
                     Set_Block_Specification (Block_Conf, Block);
                  when Iir_Kind_Indexed_Name
                    | Iir_Kind_Slice_Name =>
                     null;
                  when Iir_Kind_Parenthesis_Name =>
                     null;
                  when others =>
                     raise Internal_Error;
               end case;
            end;

         when others =>
            Error_Kind ("sem_block_configuration", Father);
      end case;

      --  LRM93 §10.1
      --  10. A block configuration
      Sem_Scopes.Open_Scope_Extension;

      --  LRM 10.3
      --  In addition, any declaration that is directly visible at the end of
      --  the declarative part of a given block is directly visible in a block
      --  configuration that configure the given block.  This rule holds unless
      --  a use clause that makes a homograph of the declaration potentially
      --  visible (see 10.4) appears in the corresponding configuration
      --  declaration, and if the scope of that use clause encompasses all or
      --  part of those configuration items.  If such a use clase appears, then
      --  the declaration will be directly visible within the corresponding
      --  configuration items, except at hose places that fall within the scope
      --  of the additional use clause.  At such places, neither name will be
      --  directly visible.
      --  FIXME: handle use clauses.
      Sem_Scopes.Extend_Scope_Of_Block_Declarations (Block);

      declare
         El : Iir;
      begin
         El := Get_Declaration_Chain (Block_Conf);
         while El /= Null_Iir loop
            case Get_Kind (El) is
               when Iir_Kind_Use_Clause =>
                  Sem_Use_Clause (El);
               when others =>
                  --  Parse checks there are only use clauses.
                  raise Internal_Error;
            end case;
            El := Get_Chain (El);
         end loop;
      end;

      --  VHDL 87: do not remove configuration specification in generate stmts.
      Clear_Instantiation_Configuration (Block, False);

      El := Get_Configuration_Item_Chain (Block_Conf);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Block_Configuration =>
               Sem_Block_Configuration (El, Block_Conf);
            when Iir_Kind_Component_Configuration =>
               Sem_Component_Configuration (El, Block_Conf);
            when others =>
               Error_Kind ("sem_block_configuration(2)", El);
         end case;
         El := Get_Chain (El);
      end loop;
      Sem_Scopes.Close_Scope_Extension;
   end Sem_Block_Configuration;

   --  LRM 1.3.2
   procedure Sem_Component_Configuration
     (Conf : Iir_Component_Configuration; Father : Iir)
   is
      Block : Iir;
      Configured_Block : Iir;
      Binding : Iir;
      Entity : Iir_Design_Unit;
      Comp : Iir_Component_Declaration;
      Primary_Entity_Aspect : Iir;
   begin
      --  LRM 10.1 Declarative Region
      --  11. A component configuration.
      Open_Declarative_Region;

      --  LRM93 §10.2
      --  If a component configuration appears as a configuration item
      --  immediatly within a block configuration that configures a given
      --  block, and the scope of a given declaration includes the end of the
      --  declarative part of that block, then the scope of the given
      --  declaration extends from the beginning to the end of the
      --  declarative region associated with the given component configuration.
      -- GHDL: this is for labels of component instantiation statements, and
      -- for local ports and generics of the component.
      if Get_Kind (Father) = Iir_Kind_Block_Configuration then
         Configured_Block := Get_Block_Specification (Father);
         if Get_Kind (Configured_Block) = Iir_Kind_Design_Unit then
            raise Internal_Error;
         end if;
         Configured_Block :=
           Get_Block_From_Block_Specification (Configured_Block);
         Sem_Scopes.Extend_Scope_Of_Block_Declarations (Configured_Block);
      else
         --  Can a component configuration not be just inside a block
         --  configuration ?
         raise Internal_Error;
      end if;
      --  FIXME: this is wrong (all declarations should be considered).
      Sem_Component_Specification
        (Configured_Block, Conf, Primary_Entity_Aspect);

      Comp := Get_Component_Name (Conf);
      if Get_Kind (Comp) /= Iir_Kind_Component_Declaration then
         --  There has been an error in sem_component_specification.
         --  Leave here.
         return;
      end if;

      --  FIXME: (todo)
      --  If a given component instance is unbound in the corresponding block,
      --  then any explicit component configuration for that instance that does
      --  not contain an explicit binding indication will contain an implicit,
      --  default binding indication (see 5.2.2).  Similarly, if a given
      --  component instance is unbound in the corresponding block, then any
      --  implicit component configuration for that instance will contain an
      --  implicit, default binding indication.
      -- FIXME: ports and generics declared by the component must be
      --   made visible here; create a declarative_region only for this purpose
      Open_Declarative_Region;
      Sem_Scopes.Add_Component_Declarations (Comp);
      Binding := Get_Binding_Indication (Conf);
      if Binding /= Null_Iir then
         Sem_Binding_Indication (Binding, Comp, Conf, Primary_Entity_Aspect);

         if Primary_Entity_Aspect /= Null_Iir then
            --  LRM93 5.2.1  Binding Indication
            --  It is an error if a formal port appears in the port map aspect
            --  of the incremental binding indication and it is a formal
            --  port that is associated with an actual other than OPEN in one
            --  of the primary binding indications.
            declare
               Inst : Iir;
               Primary_Binding : Iir;
               F_Chain : Iir;
               F_El, S_El : Iir;
               Formal : Iir;
            begin
               Inst := Get_Concurrent_Statement_Chain (Configured_Block);
               while Inst /= Null_Iir loop
                  if Get_Kind (Inst)
                    = Iir_Kind_Component_Instantiation_Statement
                    and then Get_Component_Configuration (Inst) = Conf
                  then
                     --  Check here.
                     Primary_Binding := Get_Binding_Indication
                       (Get_Configuration_Specification (Inst));
                     F_Chain := Get_Port_Map_Aspect_Chain (Primary_Binding);
                     S_El := Get_Port_Map_Aspect_Chain (Binding);
                     while S_El /= Null_Iir loop
                        --  Find S_EL formal in F_CHAIN.
                        Formal := Get_Associated_Formal (S_El);
                        F_El := F_Chain;
                        while F_El /= Null_Iir loop
                           exit when Get_Associated_Formal (F_El) = Formal;
                           F_El := Get_Chain (F_El);
                        end loop;
                        if F_El /= Null_Iir
                          and then Get_Kind (F_El)
                          /= Iir_Kind_Association_Element_Open
                        then
                           Error_Msg_Sem
                             (Disp_Node (Formal)
                              & " already associated in primary binding",
                              S_El);
                        end if;
                        S_El := Get_Chain (S_El);
                     end loop;
                  end if;
                  Inst := Get_Chain (Inst);
               end loop;
            end;
         end if;
      elsif Primary_Entity_Aspect = Null_Iir then
         --  LRM93 5.2.1
         --  If the generic map aspect or port map aspect of a primary binding
         --  indication is not present, then the default rules as described
         --  in 5.2.2 apply.

         --  Create a default binding indication.
         Entity := Get_Visible_Entity_Declaration (Comp);
         Binding := Sem_Create_Default_Binding_Indication
           (Comp, Entity, Conf, False);

         if Binding /= Null_Iir then
            --  Remap to defaults.
            Set_Default_Entity_Aspect (Binding, Get_Entity_Aspect (Binding));
            Set_Entity_Aspect (Binding, Null_Iir);

            Set_Default_Generic_Map_Aspect_Chain
              (Binding, Get_Generic_Map_Aspect_Chain (Binding));
            Set_Generic_Map_Aspect_Chain (Binding, Null_Iir);

            Set_Default_Port_Map_Aspect_Chain
              (Binding, Get_Port_Map_Aspect_Chain (Binding));
            Set_Port_Map_Aspect_Chain (Binding, Null_Iir);

            Set_Binding_Indication (Conf, Binding);
         end if;
      end if;
      Close_Declarative_Region;

      --  External block.
      Block := Get_Block_Configuration (Conf);
      if Block /= Null_Iir and then Binding /= Null_Iir then
         Sem_Block_Configuration (Block, Conf);
      end if;
      Close_Declarative_Region;
   end Sem_Component_Configuration;

   function Are_Trees_Chain_Equal (Left, Right : Iir) return Boolean
   is
      El_Left, El_Right : Iir;
   begin
      if Left = Right then
         return True;
      end if;
      El_Left := Left;
      El_Right := Right;
      loop
         if El_Left = Null_Iir and El_Right = Null_Iir then
            return True;
         end if;
         if El_Left = Null_Iir or El_Right = Null_Iir then
            return False;
         end if;
         if not Are_Trees_Equal (El_Left, El_Right) then
            return False;
         end if;
         El_Left := Get_Chain (El_Left);
         El_Right := Get_Chain (El_Right);
      end loop;
   end Are_Trees_Chain_Equal;

   --  Return TRUE iff LEFT and RIGHT are (in depth) equal.
   --  This corresponds to conformance rules, LRM93 2.7
   function Are_Trees_Equal (Left, Right : Iir) return Boolean
   is
      El_Left, El_Right : Iir;
   begin
      --  Short-cut to speed up.
      if Left = Right then
         return True;
      end if;

      --  Handle null_iir.
      if Left = Null_Iir or Right = Null_Iir then
         --  Note: LEFT *xor* RIGHT is null_iir.
         return False;
      end if;

      --  LRM 2.7  Conformance Rules
      --  A simple name can be replaced by an expanded name in which this
      --  simple name is the selector, if and only if at both places the
      --  meaning of the simple name is given by the same declaration.
      case Get_Kind (Left) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            case Get_Kind (Right) is
               when Iir_Kind_Simple_Name
                 | Iir_Kind_Selected_Name =>
                  return Are_Trees_Equal (Get_Named_Entity (Left),
                                          Get_Named_Entity (Right));
               when others =>
                  return False;
            end case;
         when others =>
            null;
      end case;

      --  If nodes are not of the same kind, then they are not equals!
      if Get_Kind (Left) /= Get_Kind (Right) then
         return False;
      end if;

      case Get_Kind (Left) is
         when Iir_Kinds_Procedure_Declaration =>
            return Are_Trees_Chain_Equal
              (Get_Interface_Declaration_Chain (Left),
               Get_Interface_Declaration_Chain (Right));
         when Iir_Kinds_Function_Declaration =>
            if Get_Return_Type (Left) /= Get_Return_Type (Right) then
               return False;
            end if;
            if Get_Pure_Flag (Left) /= Get_Pure_Flag (Right) then
               return False;
            end if;
            if not Are_Trees_Chain_Equal
              (Get_Interface_Declaration_Chain (Left),
               Get_Interface_Declaration_Chain (Right))
            then
               return False;
            end if;
            return True;
         when Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_File_Interface_Declaration =>
            if Get_Identifier (Left) /= Get_Identifier (Right) then
               return False;
            end if;
            if Get_Lexical_Layout (Left) /= Get_Lexical_Layout (Right)
              or else Get_Mode (Left) /= Get_Mode (Right)
            then
               return False;
            end if;
            if not Are_Trees_Equal (Get_Type (Left), Get_Type (Right)) then
               return False;
            end if;
            El_Left := Get_Default_Value (Left);
            El_Right := Get_Default_Value (Right);
            if (El_Left = Null_Iir) xor (El_Right = Null_Iir)  then
               return False;
            end if;
            if El_Left /= Null_Iir
              and then Are_Trees_Equal (El_Left, El_Right) = False
            then
               return False;
            end if;
            return True;

         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition =>
            if Get_Base_Type (Left) /= Get_Base_Type (Right)
              or else Get_Resolution_Function (Left)
              /= Get_Resolution_Function (Right)
            then
               return False;
            end if;
            if Get_Type_Declarator (Left) /= Get_Type_Declarator (Right) then
               return False;
            end if;
            if Are_Trees_Equal (Get_Range_Constraint (Left),
                                Get_Range_Constraint (Right)) = False
            then
               return False;
            end if;
            return True;
         when Iir_Kind_Array_Subtype_Definition =>
            if Get_Base_Type (Left) /= Get_Base_Type (Right)
              or else (Get_Resolution_Function (Left)
                       /= Get_Resolution_Function (Right))
            then
               return False;
            end if;
            declare
               L_Left, L_Right : Iir_List;
            begin
               L_Left := Get_Index_Subtype_List (Left);
               L_Right := Get_Index_Subtype_List (Right);
               for I in Natural loop
                  El_Left := Get_Nth_Element (L_Left, I);
                  El_Right := Get_Nth_Element (L_Right, I);
                  exit when El_Left = Null_Iir;
                  if not Are_Trees_Equal (El_Left, El_Right) then
                     return False;
                  end if;
               end loop;
            end;
            return True;

         when Iir_Kind_Integer_Literal
           | Iir_Kind_Enumeration_Literal =>
            if Get_Value (Left) /= Get_Value (Right) then
               return False;
            end if;
            return Are_Trees_Equal (Get_Literal_Origin (Left),
                                    Get_Literal_Origin (Right));
         when Iir_Kind_Physical_Int_Literal =>
            if Get_Value (Left) /= Get_Value (Right)
              or else Get_Unit_Name (Left) /= Get_Unit_Name (Right)
            then
               return False;
            end if;
            return Are_Trees_Equal (Get_Literal_Origin (Left),
                                    Get_Literal_Origin (Right));
         when Iir_Kind_Physical_Fp_Literal =>
            if Get_Fp_Value (Left) /= Get_Fp_Value (Right)
              or else Get_Unit_Name (Left) /= Get_Unit_Name (Right)
            then
               return False;
            end if;
            return Are_Trees_Equal (Get_Literal_Origin (Left),
                                    Get_Literal_Origin (Right));
         when Iir_Kind_Floating_Point_Literal =>
            if Get_Fp_Value (Left) /= Get_Fp_Value (Right) then
               return False;
            end if;
            return Are_Trees_Equal (Get_Literal_Origin (Left),
                                    Get_Literal_Origin (Right));

         when Iir_Kinds_Dyadic_Operator =>
            return Are_Trees_Equal (Get_Left (Left), Get_Left (Right))
              and then Are_Trees_Equal (Get_Right (Left), Get_Right (Right));
         when Iir_Kinds_Monadic_Operator =>
            return Are_Trees_Equal (Get_Operand (Left), Get_Operand (Right));

         when Iir_Kind_Access_Type_Definition
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_File_Type_Definition =>
            return Left = Right;

         when Iir_Kind_Range_Expression =>
            if Get_Type (Left) /= Get_Type (Right)
              or else Get_Direction (Left) /= Get_Direction (Right)
            then
               return False;
            end if;
            if not Are_Trees_Equal (Get_Left_Limit (Left),
                                    Get_Left_Limit (Right))
              or else not Are_Trees_Equal (Get_Right_Limit (Left),
                                           Get_Right_Limit (Right))
            then
               return False;
            end if;
            return True;

         when Iir_Kind_High_Type_Attribute
           | Iir_Kind_Low_Type_Attribute
           | Iir_Kind_Left_Type_Attribute
           | Iir_Kind_Right_Type_Attribute
           | Iir_Kind_Ascending_Type_Attribute =>
            return Are_Trees_Equal (Get_Prefix (Left), Get_Prefix (Right));

         when Iir_Kind_String_Literal
           | Iir_Kind_Bit_String_Literal =>
            if Get_Kind (Left) = Iir_Kind_Bit_String_Literal
              and then Get_Bit_String_Base (Left)
              /= Get_Bit_String_Base (Right)
            then
               return False;
            end if;
            declare
               use Str_Table;
               Len : Nat32;
               L_Ptr : String_Fat_Acc;
               R_Ptr : String_Fat_Acc;
            begin
               Len := Get_String_Length (Left);
               if Get_String_Length (Right) /= Len then
                  return False;
               end if;
               L_Ptr := Get_String_Fat_Acc (Get_String_Id (Left));
               R_Ptr := Get_String_Fat_Acc (Get_String_Id (Right));
               for I in 1 .. Len loop
                  if L_Ptr (I) /= R_Ptr (I) then
                     return False;
                  end if;
               end loop;
               return True;
            end;

         when Iir_Kind_Aggregate =>
            if not Are_Trees_Equal (Get_Type (Left), Get_Type (Right)) then
               return False;
            end if;
            declare
               El_L, El_R : Iir;
            begin
               El_L := Get_Association_Choices_Chain (Left);
               El_R := Get_Association_Choices_Chain (Right);
               loop
                  exit when El_L = Null_Iir and El_R = Null_Iir;
                  if not Are_Trees_Equal (El_L, El_R) then
                     return False;
                  end if;
                  El_L := Get_Chain (El_L);
                  El_R := Get_Chain (El_R);
               end loop;
               return True;
            end;

         when Iir_Kind_Choice_By_None
              | Iir_Kind_Choice_By_Others =>
            return Are_Trees_Equal (Get_Associated (Left),
                                    Get_Associated (Right));
         when Iir_Kind_Choice_By_Name =>
            if not Are_Trees_Equal (Get_Name (Left), Get_Name (Right)) then
               return False;
            end if;
            return Are_Trees_Equal (Get_Associated (Left),
                                    Get_Associated (Right));
         when Iir_Kind_Choice_By_Expression
           | Iir_Kind_Choice_By_Range =>
            if not Are_Trees_Equal (Get_Expression (Left),
                                    Get_Expression (Right)) then
               return False;
            end if;
            return Are_Trees_Equal (Get_Associated (Left),
                                    Get_Associated (Right));
         when others =>
            Error_Kind ("are_trees_equal", Left);
      end case;
   end Are_Trees_Equal;

   --  LRM 2.7  Conformance Rules.
   procedure Check_Conformance_Rules (Subprg, Spec: Iir) is
   begin
      if not Are_Trees_Equal (Subprg, Spec) then
         --  FIXME: should explain why it does not conform ?
         Error_Msg_Sem ("body does not conform with specification at "
                        & Disp_Location (Spec), Subprg);
      end if;
   end Check_Conformance_Rules;

   -- Return the specification corresponding to a declaration DECL, or
   -- null_Iir if none.
   -- FIXME: respect rules of LRM93 2.7
   function Find_Subprogram_Specification (Decl: Iir) return Iir
   is
      Interpretation : Name_Interpretation_Type;
      Decl1: Iir;
      Hash : Iir_Int32;
      Kind : Iir_Kind;
   begin
      Hash := Get_Subprogram_Hash (Decl);
      Interpretation := Get_Interpretation (Get_Identifier (Decl));
      while Valid_Interpretation (Interpretation) loop
         if not Is_In_Current_Declarative_Region (Interpretation) then
            --  The declaration does not belong to the current declarative
            --  region, neither will the following one.  So, we do not found
            --  it.
            return Null_Iir;
         end if;
         Decl1 := Get_Declaration (Interpretation);
         Kind := Get_Kind (Decl1);
         --  Should be sure DECL1 and DECL belongs to the same declarative
         --  region, ie DECL1 was not made visible via a USE clause.
         --
         --  Also, only check for explicitly subprograms (and not
         --  implicit one).
         if (Kind = Iir_Kind_Function_Declaration
             or Kind = Iir_Kind_Procedure_Declaration)
           and then not Is_Potentially_Visible (Interpretation)
           and then Get_Subprogram_Hash (Decl1) = Hash
           and then Is_Same_Profile (Decl, Decl1)
         then
            return Decl1;
         end if;
         Interpretation := Get_Next_Interpretation (Interpretation);
      end loop;
      return Null_Iir;
   end Find_Subprogram_Specification;

   procedure Set_Subprogram_Overload_Number (Decl : Iir)
   is
      Inter : Name_Interpretation_Type;
      Prev : Iir;
      Num : Iir_Int32;
   begin
      Inter := Get_Interpretation (Get_Identifier (Decl));
      while Valid_Interpretation (Inter)
        and then Is_In_Current_Declarative_Region (Inter)
      loop
         --  There is a previous declaration with the same name in the
         --  current declarative region.
         Prev := Get_Declaration (Inter);
         case Get_Kind (Prev) is
            when Iir_Kind_Function_Declaration
              | Iir_Kind_Procedure_Declaration =>
               --  The previous declaration is a user subprogram.
               Num := Get_Overload_Number (Prev) + 1;
               if Num = 1
                 and then Get_Parent (Prev) = Get_Parent (Decl)
               then
                  --  The previous was not (yet) overloaded.  Mark it as
                  --  overloaded.
                  --  Do not mark it if it is not in the same declarative part.
                  --  (ie, do not change a subprogram declaration in the
                  --   package while analyzing the body).
                  Set_Overload_Number (Prev, 1);
                  Num := 2;
               end if;
               Set_Overload_Number (Decl, Num);
               return;
            when Iir_Kind_Implicit_Function_Declaration
              | Iir_Kind_Implicit_Procedure_Declaration =>
               --  Implicit declarations aren't taken into account (as they
               --  are mangled differently).
               Inter := Get_Next_Interpretation (Inter);
            when others =>
               --  Can be an enumeration literal or an error.
               Set_Overload_Number (Decl, 0);
               return;
         end case;
      end loop;
      --  No previous declaration in the current declarative region.
      Set_Overload_Number (Decl, 0);
   end Set_Subprogram_Overload_Number;

   --  Check requirements on number of interfaces for subprogram specification
   --  SUBPRG.  Requirements only concern operators, and are defined in
   --  LRM 2.3.1
   procedure Check_Operator_Requirements (Id : Name_Id; Subprg : Iir)
   is
      use Std_Names;

      Nbr_Interfaces : Natural;
      Is_Method : Boolean;
   begin
      Nbr_Interfaces := Iir_Chains.Get_Chain_Length
        (Get_Interface_Declaration_Chain (Subprg));

      --  For vhdl-02, the protected variable is an implicit parameter.
      if Flags.Vhdl_Std >= Vhdl_02
        and then Is_Subprogram_Method (Subprg)
      then
         Nbr_Interfaces := Nbr_Interfaces + 1;
      else
         Is_Method := False;
      end if;

      case Id is
         when Name_Abs
           | Name_Not =>
            --  LRM93 2.3.1
            --  The subprogram specification of a unary operator must have a
            --  single parameter.

            --  LRM02 2.3.1
            --  ..., unless the subprogram specification is a method (see
            --  3.5.1) of a protected type.  In this latter case, the
            --  subprogram specification must have no parameters.
            if Nbr_Interfaces = 1 then
               return;
            end if;
            Error_Msg_Sem ("unary operator must have a single parameter",
                           Subprg);
         when Name_Mod
           | Name_Rem
           | Name_Op_Mul
           | Name_Op_Div
           | Name_Relational_Operators
           | Name_Op_Concatenation
           | Name_Shift_Operators
           | Name_Op_Exp =>
            --  LRM93 2.3.1
            --  The subprogram specification of a binary operator must have
            --  two parameters.

            --  LRM02 2.3.1
            --  ..., unless the subprogram specification is a method of a
            --  protected type, in which case, the subprogram specification
            --  must have a single parameter.
            if Nbr_Interfaces = 2 then
               return;
            end if;
            Error_Msg_Sem
              ("binary operators must have two parameters", Subprg);
         when Name_Logical_Operators
           | Name_Xnor =>
            --  LRM08 4.5.2 Operator overloading
            --  For each of the "+", "-", "and", "or", "xor", "nand", "nor"
            --  and "xnor", overloading is allowed both as a unary operator
            --  and as a binary operator.
            if Nbr_Interfaces = 2 then
               return;
            end if;
            if Nbr_Interfaces = 1 then
               if Vhdl_Std >= Vhdl_08 then
                  return;
               end if;
               Error_Msg_Sem
                 ("logical operators must have two parameters before vhdl08",
                  Subprg);
            else
               Error_Msg_Sem
                 ("logical operators must have two parameters", Subprg);
            end if;
         when Name_Op_Plus
           | Name_Op_Minus =>
            --  LRM93 2.3.1
            --  For each of the operators "+" and "-", overloading is allowed
            --  both as a unary operator and as a binary operator.
            if Nbr_Interfaces in 1 .. 2 then
               return;
            end if;
            Error_Msg_Sem
              ("""+"" and ""-"" operators must have 1 or 2 parameters",
               Subprg);
         when others =>
            return;
      end case;
      if Is_Method then
         Error_Msg_Sem
           (" (the protected object is an implicit parameter of methods)",
            Subprg);
      end if;
   end Check_Operator_Requirements;

   procedure Compute_Subprogram_Hash (Subprg : Iir)
   is
      type Hash_Type is mod 2**32;
      function To_Hash is new Ada.Unchecked_Conversion
        (Source => Iir, Target => Hash_Type);
      function To_Int32 is new Ada.Unchecked_Conversion
        (Source => Hash_Type, Target => Iir_Int32);

      Kind : Iir_Kind;
      Hash : Hash_Type;
      Sig : Hash_Type;
      Inter : Iir;
      Itype : Iir;
   begin
      Kind := Get_Kind (Subprg);
      if Kind in Iir_Kinds_Function_Declaration
        or else Kind = Iir_Kind_Enumeration_Literal
      then
         Itype := Get_Base_Type (Get_Return_Type (Subprg));
         Hash := To_Hash (Itype);
         Sig := 8;
      else
         Sig := 1;
         Hash := 0;
      end if;

      if Kind /= Iir_Kind_Enumeration_Literal then
         Inter := Get_Interface_Declaration_Chain (Subprg);
         while Inter /= Null_Iir loop
            Itype := Get_Base_Type (Get_Type (Inter));
            Sig := Sig + 1;
            Hash := Hash * 7 + To_Hash (Itype);
            Hash := Hash + Hash / 2**28;
            Inter := Get_Chain (Inter);
         end loop;
      end if;
      Set_Subprogram_Hash (Subprg, To_Int32 (Hash + Sig));
   end Compute_Subprogram_Hash;

   --  LRM 2.1  Subprogram Declarations.
   function Sem_Subprogram_Declaration (Subprg: Iir) return Iir
   is
      Spec: Iir;
      Interface_Chain : Iir;
      Subprg_Body : Iir;
   begin
      --  Set depth.
      declare
         Parent : constant Iir := Get_Parent (Subprg);
      begin
         case Get_Kind (Parent) is
            when Iir_Kind_Function_Declaration
              | Iir_Kind_Procedure_Declaration =>
               raise Internal_Error;
            when Iir_Kind_Function_Body
              | Iir_Kind_Procedure_Body =>
               Set_Subprogram_Depth
                 (Subprg,
                  Get_Subprogram_Depth
                  (Get_Subprogram_Specification (Parent)) + 1);
            when others =>
               Set_Subprogram_Depth (Subprg, 0);
         end case;
      end;

      --  LRM 10.1 Declarative Region
      --  3. A subprogram declaration, together with the corresponding
      --     subprogram body.
      Open_Declarative_Region;

      --  Sem interfaces.
      Interface_Chain := Get_Interface_Declaration_Chain (Subprg);
      case Get_Kind (Subprg) is
         when Iir_Kind_Function_Declaration =>
            Sem_Interface_Chain (Interface_Chain, Interface_Function);
            Set_Return_Type
              (Subprg, Sem_Subtype_Indication (Get_Return_Type (Subprg)));
            Set_All_Sensitized_State (Subprg, Unknown);
         when Iir_Kind_Procedure_Declaration =>
            Sem_Interface_Chain (Interface_Chain, Interface_Procedure);
            --  Unless the body is analyzed, the procedure purity is unknown.
            Set_Purity_State (Subprg, Unknown);
            --  Check if the procedure is passive.
            Set_Passive_Flag (Subprg, True);
            Set_All_Sensitized_State (Subprg, Unknown);
            declare
               Inter : Iir;
            begin
               Inter := Interface_Chain;
               while Inter /= Null_Iir loop
                  if Get_Kind (Inter) = Iir_Kind_Signal_Interface_Declaration
                    and then Get_Mode (Inter) /= Iir_In_Mode
                  then
                     --  There is a driver for this signal interface.
                     Set_Passive_Flag (Subprg, False);
                     exit;
                  end if;
                  Inter := Get_Chain (Inter);
               end loop;
            end;
         when others =>
            Error_Kind ("sem_subprogram_declaration", Subprg);
      end case;

      Check_Operator_Requirements (Get_Identifier (Subprg), Subprg);

      Compute_Subprogram_Hash (Subprg);

      --  The specification has been semantized, close the declarative region
      --  now.
      Close_Declarative_Region;

      Subprg_Body := Get_Chain (Subprg);
      if Subprg_Body /= Null_Iir
        and then (Get_Kind (Subprg_Body) = Iir_Kind_Function_Body
                  or else Get_Kind (Subprg_Body) = Iir_Kind_Procedure_Body)
      then
         Spec := Find_Subprogram_Specification (Subprg);
      else
         Spec := Null_Iir;
      end if;

      if Spec /= Null_Iir then
         -- SUBPRG is the body of the specification SPEC.
         Check_Conformance_Rules (Subprg, Spec);
         Xref_Body (Subprg, Spec);
         Free_Old_Iir (Subprg);
         Set_Subprogram_Specification (Subprg_Body, Spec);
         Set_Subprogram_Body (Spec, Subprg_Body);
         return Subprg_Body;
      else
         --  Forward declaration or specification followed by body.
         Set_Subprogram_Overload_Number (Subprg);
         Sem_Scopes.Add_Name (Subprg);
         Name_Visible (Subprg);
         Xref_Decl (Subprg);
         return Subprg;
      end if;
   end Sem_Subprogram_Declaration;

   procedure Add_Analysis_Checks_List (El : Iir)
   is
      Design : constant Iir := Get_Current_Design_Unit;
      List : Iir_List;
   begin
      List := Get_Analysis_Checks_List (Design);
      if List = Null_Iir_List then
         List := Create_Iir_List;
         Set_Analysis_Checks_List (Design, List);
      end if;
      Add_Element (List, El);
   end Add_Analysis_Checks_List;

   procedure Sem_Subprogram_Body (Subprg : Iir)
   is
      Spec : Iir;
      El : Iir;
   begin
      Spec := Get_Subprogram_Specification (Subprg);
      Set_Impure_Depth (Subprg, Iir_Depth_Pure);

      --  LRM 10.1  Declarative regions
      --  3.  A subprogram declaration, together with the corresponding
      --     subprogram body.
      Open_Declarative_Region;
      Set_Is_Within_Flag (Spec, True);

      -- Add the interface names into the current declarative region.
      El := Get_Interface_Declaration_Chain (Spec);
      while El /= Null_Iir loop
         Add_Name (El, Get_Identifier (El), False);
         if Get_Kind (El) = Iir_Kind_Signal_Interface_Declaration then
            Set_Has_Active_Flag (El, False);
         end if;
         El := Get_Chain (El);
      end loop;

      Sem_Sequential_Statements (Spec, Subprg);

      Set_Is_Within_Flag (Spec, False);
      Close_Declarative_Region;

      case Get_Kind (Spec) is
         when Iir_Kind_Procedure_Declaration =>
            --  Update purity state of procedure if there are no callees.
            case Get_Purity_State (Spec) is
               when Pure
                 | Maybe_Impure =>
                  --  We can't know this yet.
                  raise Internal_Error;
               when Impure =>
                  null;
               when Unknown =>
                  if Get_Callees_List (Spec) = Null_Iir_List then
                     --  Since there are no callees, purity state can
                     --  be updated.
                     if Get_Impure_Depth (Subprg) = Iir_Depth_Pure then
                        Set_Purity_State (Spec, Pure);
                     else
                        Set_Purity_State (Spec, Maybe_Impure);
                     end if;
                  end if;
            end case;

            --  Update wait state if the state of all callees is known.
            if Get_Wait_State (Spec) = Unknown then
               declare
                  Callees : Iir_List;
                  Callee : Iir;
                  State : Tri_State_Type;
               begin
                  Callees := Get_Callees_List (Spec);
                  --  Per default, has no wait.
                  Set_Wait_State (Spec, False);
                  if Callees /= Null_Iir_List then
                     for I in Natural loop
                        Callee := Get_Nth_Element (Callees, I);
                        exit when Callee = Null_Iir;
                        case Get_Kind (Callee) is
                           when Iir_Kinds_Function_Declaration =>
                              null;
                           when Iir_Kind_Procedure_Declaration =>
                              State := Get_Wait_State (Callee);
                              case State is
                                 when False =>
                                    null;
                                 when Unknown =>
                                    --  Yet unknown, but can be TRUE.
                                    Set_Wait_State (Spec, Unknown);
                                 when True =>
                                    --  Can this happen ?
                                    raise Internal_Error;
                                    --Set_Wait_State (Spec, True);
                                    --exit;
                              end case;
                           when Iir_Kind_Implicit_Procedure_Declaration =>
                              null;
                           when others =>
                              Error_Kind ("sem_subprogram_body(2)", Callee);
                        end case;
                     end loop;
                  end if;
               end;
            end if;

            --  Set All_Sensitized_State in trivial cases.
            if Get_All_Sensitized_State (Spec) = Unknown
              and then Get_Callees_List (Spec) = Null_Iir_List
            then
               Set_All_Sensitized_State (Spec, No_Signal);
            end if;

            --  Do not add to Analysis_Check_List as procedures can't
            --  generate purity/wait/all-sensitized errors by themselves.

         when Iir_Kind_Function_Declaration =>
            if Get_Callees_List (Spec) /= Null_Iir_List then
               --  Purity calls to be checked later.
               --  No wait statements in procedures called.
               Add_Analysis_Checks_List (Spec);
            end if;
         when others =>
            Error_Kind ("sem_subprogram_body", Spec);
      end case;
   end Sem_Subprogram_Body;

   --  Status of Update_And_Check_Pure_Wait.
   type Update_Pure_Status is
     (
      --  The purity/wait/all-sensitized are computed and known.
      Update_Pure_Done,
      --  A missing body prevents from computing the purity/wait/all-sensitized
      Update_Pure_Missing,
      --  Purity/wait/all-sensitized is unknown (recursion).
      Update_Pure_Unknown
     );
   function Update_And_Check_Pure_Wait (Subprg : Iir)
                                       return Update_Pure_Status
   is
      procedure Error_Wait (Caller : Iir; Callee : Iir) is
      begin
         Error_Msg_Sem
           (Disp_Node (Caller) & " must not contain wait statement, but calls",
            Caller);
         Error_Msg_Sem
           (Disp_Node (Callee) & " which has (indirectly) a wait statement",
            Callee);
      end Error_Wait;

      --  Kind of subprg.
      type Caller_Kind is (K_Function, K_Process, K_Procedure);
      Kind : Caller_Kind;

      Callees_List : Iir_List := Get_Callees_List (Subprg);
      Callee : Iir;
      Callee_Bod : Iir;
      Subprg_Depth : Iir_Int32;
      Subprg_Bod : Iir;
      --  Current purity depth of SUBPRG.
      Depth : Iir_Int32;
      Depth_Callee : Iir_Int32;
      Has_Wait_Errors : Boolean := False;
      Npos : Natural;
      Res, Res1 : Update_Pure_Status;
   begin
      case Get_Kind (Subprg) is
         when Iir_Kind_Function_Declaration =>
            Kind := K_Function;
            Subprg_Bod := Null_Iir;
            Subprg_Depth := Get_Subprogram_Depth (Subprg);
            if Get_Pure_Flag (Subprg) then
               Depth := Iir_Depth_Pure;
            else
               Depth := Iir_Depth_Impure;
            end if;

         when Iir_Kind_Procedure_Declaration =>
            Kind := K_Procedure;
            if Get_Purity_State (Subprg) = Impure
              and then Get_Wait_State (Subprg) /= Unknown
              and then Get_All_Sensitized_State (Subprg) /= Unknown
            then
               --  No need to go further.
               if Get_All_Sensitized_State (Subprg) = No_Signal
                 or else Vhdl_Std < Vhdl_08
               then
                  Destroy_Iir_List (Callees_List);
                  Set_Callees_List (Subprg, Null_Iir_List);
               end if;
               return Update_Pure_Done;
            end if;
            Subprg_Bod := Get_Subprogram_Body (Subprg);
            Subprg_Depth := Get_Subprogram_Depth (Subprg);
            Depth := Get_Impure_Depth (Subprg_Bod);

         when Iir_Kind_Sensitized_Process_Statement =>
            Kind := K_Process;
            Subprg_Bod := Null_Iir;
            Subprg_Depth := Iir_Depth_Top;
            Depth := Iir_Depth_Impure;

         when others =>
            Error_Kind ("update_and_check_pure_wait(1)", Subprg);
      end case;

      --  If the subprogram has no callee list, there is nothing to do.
      if Callees_List = Null_Iir_List then
         --  There are two reasons why a callees_list is null:
         --  * either because SUBPRG does not call any procedure
         --    in this case, the status are already known and we should have
         --    returned in the above case.
         --  * or because of a recursion
         --    in this case the status are still unknown here.
         return Update_Pure_Unknown;
      end if;

      --  By default we don't know the status.
      Res := Update_Pure_Unknown;

      --  This subprogram is being considered.
      --  To avoid infinite loop, suppress its callees list.
      Set_Callees_List (Subprg, Null_Iir_List);

      --  First loop: check without recursion.
      --  Second loop: recurse if necessary.
      for J in 0 .. 1 loop
         Npos := 0;
         for I in Natural loop
            Callee := Get_Nth_Element (Callees_List, I);
            exit when Callee = Null_Iir;

            --  Note:
            --  Pure functions should not be in the list.
            --  Impure functions must have directly set Purity_State.

            --  Check pure.
            Callee_Bod := Get_Subprogram_Body (Callee);
            if Callee_Bod = Null_Iir then
               --  No body yet for the subprogram called.
               --  Nothing can be extracted from it, postpone the checks until
               --  elaboration.
               Res := Update_Pure_Missing;
            else
               --  Second loop: recurse if a state is not known.
               if J = 1
                 and then
                 (Get_Purity_State (Callee) = Unknown
                    or else Get_Wait_State (Callee) = Unknown
                    or else Get_All_Sensitized_State (Callee) = Unknown)
               then
                  Res1 := Update_And_Check_Pure_Wait (Callee);
                  if Res1 = Update_Pure_Missing then
                     Res := Update_Pure_Missing;
                  end if;
               end if;

               --  Check purity only if the subprogram is not impure.
               if Depth /= Iir_Depth_Impure then
                  Depth_Callee := Get_Impure_Depth (Callee_Bod);

                  --  Check purity depth.
                  if Depth_Callee < Subprg_Depth then
                     --  The call is an impure call because it calls an outer
                     --   subprogram (or an impure subprogram).
                     --  FIXME: check the compare.
                     Depth_Callee := Iir_Depth_Impure;
                     if Kind = K_Function then
                        Error_Pure (Subprg, Callee, Null_Iir);
                     end if;
                  end if;

                  --  Update purity depth.
                  if Depth_Callee < Depth then
                     Depth := Depth_Callee;
                     if Kind = K_Procedure then
                        --  Update for recursivity.
                        Set_Impure_Depth (Subprg_Bod, Depth);
                        if Depth = Iir_Depth_Impure then
                           Set_Purity_State (Subprg, Impure);
                        end if;
                     end if;
                  end if;
               end if;
            end if;

            --  Check wait.
            if Has_Wait_Errors = False
              and then Get_Wait_State (Callee) = True
            then
               if Kind = K_Procedure then
                  Set_Wait_State (Subprg, True);
               else
                  Error_Wait (Subprg, Callee);
                  Has_Wait_Errors := True;
               end if;
            end if;

            if Get_All_Sensitized_State (Callee) = Invalid_Signal then
               case Kind is
                  when K_Function | K_Procedure =>
                     Set_All_Sensitized_State (Subprg, Invalid_Signal);
                  when K_Process =>
                     --  LRM08 11.3
                     --
                     --  It is an error if a process statement with the
                     --  reserved word ALL as its process sensitivity list
                     --  is the parent of a subprogram declared in a design
                     --  unit other than that containing the process statement
                     --  and the subprogram reads an explicitly declared
                     --  signal that is not a formal signal parameter or
                     --  member of a formal signal parameter of the
                     --  subprogram or of any of its parents.  Similarly,
                     --  it is an error if such subprogram reads an implicit
                     --  signal whose explicit ancestor is not a formal signal
                     --  parameter or member of a formal parameter of
                     --  the subprogram or of any of its parents.
                     Error_Msg_Sem
                       ("all-sensitized " & Disp_Node (Subprg)
                          & " can't call " & Disp_Node (Callee), Subprg);
                     Error_Msg_Sem
                       (" (as this subprogram reads (indirectly) a signal)",
                        Subprg);
               end case;
            end if;

            --  Keep in list.
            if Callee_Bod = Null_Iir
              or else
              (Get_Purity_State (Callee) = Unknown
                 and then Depth /= Iir_Depth_Impure)
              or else
              (Get_Wait_State (Callee) = Unknown
                 and then (Kind /= K_Procedure
                             or else Get_Wait_State (Subprg) = Unknown))
              or else
              (Vhdl_Std >= Vhdl_08
                 and then
                 (Get_All_Sensitized_State (Callee) = Unknown
                    or else Get_All_Sensitized_State (Callee) = Read_Signal))
            then
               Replace_Nth_Element (Callees_List, Npos, Callee);
               Npos := Npos + 1;
            end if;
         end loop;

         --  End of callee loop.
         if Npos = 0 then
            Destroy_Iir_List (Callees_List);
            Callees_List := Null_Iir_List;
            if Kind = K_Procedure then
               if Get_Purity_State (Subprg) = Unknown then
                  Set_Purity_State (Subprg, Maybe_Impure);
               end if;
               if Get_Wait_State (Subprg) = Unknown then
                  Set_Wait_State (Subprg, False);
               end if;
            end if;
            if Kind = K_Procedure or Kind = K_Function then
               if Get_All_Sensitized_State (Subprg) = Unknown then
                  Set_All_Sensitized_State (Subprg, No_Signal);
               end if;
            end if;
            Res := Update_Pure_Done;
            exit;
         else
            Set_Nbr_Elements (Callees_List, Npos);
         end if;
      end loop;

      Set_Callees_List (Subprg, Callees_List);

      return Res;
   end Update_And_Check_Pure_Wait;

   --  Check pure/wait/all-sensitized issues for SUBPRG (subprogram or
   --  process).  Return False if the analysis is incomplete (and must
   --  be deferred).
   function Root_Update_And_Check_Pure_Wait (Subprg : Iir) return Boolean
   is
      Res : Update_Pure_Status;
   begin
      Res := Update_And_Check_Pure_Wait (Subprg);
      case Res is
         when Update_Pure_Done =>
            return True;
         when Update_Pure_Missing =>
            return False;
         when Update_Pure_Unknown =>
            --  The purity/wait is unknown, but all callee were walked.
            --  This means there are recursive calls but without violations.
            if Get_Kind (Subprg) = Iir_Kind_Procedure_Declaration then
               if Get_Purity_State (Subprg) = Unknown then
                  Set_Purity_State (Subprg, Maybe_Impure);
               end if;
               if Get_Wait_State (Subprg) = Unknown then
                  Set_Wait_State (Subprg, False);
               end if;
            end if;
            if Get_Kind (Subprg) in Iir_Kinds_Subprogram_Declaration then
               if Get_All_Sensitized_State (Subprg) = Unknown then
                  Set_All_Sensitized_State (Subprg, No_Signal);
               end if;
            end if;
            return True;
      end case;
   end Root_Update_And_Check_Pure_Wait;

   procedure Sem_Analysis_Checks_List (Unit : Iir_Design_Unit;
                                       Emit_Warnings : Boolean)
   is
      List : Iir_List := Get_Analysis_Checks_List (Unit);
      El : Iir;
      Npos : Natural;
      Keep : Boolean;
      Callees : Iir_List;
      Callee : Iir;
   begin
      if List = Null_Iir_List then
         return;
      end if;
      Npos := 0;
      for I in Natural loop
         El := Get_Nth_Element (List, I);
         exit when El = Null_Iir;
         Keep := False;
         case Get_Kind (El) is
            when Iir_Kind_Function_Declaration =>
               --  FIXME: remove from list if fully tested ?
               if not Root_Update_And_Check_Pure_Wait (El) then
                  Keep := True;
                  if Emit_Warnings then
                     Callees := Get_Callees_List (El);
                     if Callees = Null_Iir_List then
                        raise Internal_Error;
                     end if;
                     Warning_Msg_Sem
                       ("can't assert that all calls in " & Disp_Node (El)
                        & " are pure or have not wait; "
                        & "will be checked at elaboration", El);
                     Callee := Get_Nth_Element (Callees, 0);
                     --  FIXME: could improve this message by displaying the
                     --  chain of calls until the first subprograms in
                     --  unknown state.
                     Warning_Msg_Sem
                       ("(first such call is to " & Disp_Node (Callee) & ")",
                        Callee);
                  end if;
               end if;
            when Iir_Kind_Sensitized_Process_Statement =>
               if not Root_Update_And_Check_Pure_Wait (El) then
                  Keep := True;
                  if Emit_Warnings then
                     Warning_Msg_Sem
                       ("can't assert that " & Disp_Node (El)
                        & " has not wait; will be checked at elaboration", El);
                  end if;
               end if;
            when others =>
               Error_Kind ("sem_analysis_checks_list", El);
         end case;
         if Keep then
            Replace_Nth_Element (List, Npos, El);
            Npos := Npos + 1;
         end if;
      end loop;
      if Npos = 0 then
         Destroy_Iir_List (List);
         Set_Analysis_Checks_List (Unit, Null_Iir_List);
      else
         Set_Nbr_Elements (List, Npos);
      end if;
   end Sem_Analysis_Checks_List;

   -- Return true if package declaration DECL needs a body.
   -- Ie, it contains subprogram specification or deferred constants.
   function Package_Need_Body_P (Decl: Iir_Package_Declaration)
     return Boolean
   is
      El: Iir;
      Def : Iir;
   begin
      El := Get_Declaration_Chain (Decl);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Function_Declaration
              | Iir_Kind_Procedure_Declaration =>
               return True;
            when Iir_Kind_Constant_Declaration =>
               if Get_Default_Value (El) = Null_Iir then
                  return True;
               end if;
            when Iir_Kind_Variable_Declaration
              | Iir_Kind_File_Declaration
              | Iir_Kind_Signal_Declaration
              | Iir_Kind_Object_Alias_Declaration
              | Iir_Kind_Non_Object_Alias_Declaration
              | Iir_Kind_Group_Template_Declaration
              | Iir_Kind_Group_Declaration =>
               null;
            when Iir_Kind_Type_Declaration =>
               Def := Get_Type (El);
               if Def /= Null_Iir
                 and then Get_Kind (Def) = Iir_Kind_Protected_Type_Declaration
               then
                  return True;
               end if;
            when Iir_Kind_Anonymous_Type_Declaration
              | Iir_Kind_Subtype_Declaration =>
               null;
            when Iir_Kind_Implicit_Function_Declaration
              | Iir_Kind_Implicit_Procedure_Declaration =>
               null;
            when Iir_Kind_Attribute_Declaration
              | Iir_Kind_Attribute_Specification =>
               null;
            when Iir_Kind_Use_Clause =>
               null;
            when Iir_Kind_Component_Declaration =>
               null;
            when Iir_Kind_Protected_Type_Body =>
               null;
            when others =>
               Error_Kind ("package_need_body_p", El);
         end case;
         El := Get_Chain (El);
      end loop;
      return False;
   end Package_Need_Body_P;

   --  LRM 2.5  Package Declarations.
   procedure Sem_Package_Declaration (Decl: Iir_Package_Declaration)
   is
      Unit : Iir_Design_Unit;
      Implicit : Implicit_Signal_Declaration_Type;
   begin
      Unit := Get_Design_Unit (Decl);
      Sem_Scopes.Add_Name (Unit);
      Set_Visible_Flag (Unit, True);
      Xref_Decl (Decl);

      --  LRM93 10.1 Declarative Region
      --  4. A package declaration, together with the corresponding
      --     body (if any).
      Open_Declarative_Region;

      Push_Signals_Declarative_Part (Implicit, Decl);

      Sem_Declaration_Chain (Decl, not Flags.Flag_Whole_Analyze);
      --  GHDL: subprogram bodies appear in package body.

      Pop_Signals_Declarative_Part (Implicit);
      Close_Declarative_Region;
      Set_Need_Body (Decl, Package_Need_Body_P (Decl));
   end Sem_Package_Declaration;

   --  LRM 2.6  Package Bodies.
   procedure Sem_Package_Body (Decl: Iir)
   is
      Package_Ident: Name_Id;
      Design_Unit: Iir_Design_Unit;
      Package_Decl: Iir;
   begin
      -- First, find the package declaration.
      Package_Ident := Get_Identifier (Decl);
      Design_Unit := Libraries.Load_Primary_Unit
        (Get_Library (Get_Design_File (Get_Current_Design_Unit)),
         Package_Ident, Decl);
      if Design_Unit = Null_Iir then
         Error_Msg_Sem ("package '" & Name_Table.Image (Package_Ident)
                        & "' was not analysed",
                        Decl);
         return;
      end if;
      Package_Decl := Get_Library_Unit (Design_Unit);
      if Get_Kind (Package_Decl) /= Iir_Kind_Package_Declaration then
         Error_Msg_Sem
           ("primary unit '" & Name_Table.Image (Package_Ident)
            & "' is not a package", Decl);
         return;
      end if;

      --  Emit a warning is a body is not necessary.
      if not Get_Need_Body (Package_Decl) then
         Warning_Msg_Sem
           (Disp_Node (Package_Decl) & " does not require a body", Decl);
      end if;

      Set_Package (Decl, Package_Decl);
      Xref_Body (Decl, Package_Decl);
      Set_Package_Body (Package_Decl, Decl);
      Add_Dependence (Design_Unit);

      Add_Name (Design_Unit);

      --  Add the context clauses from the primary unit.
      Add_Context_Clauses (Design_Unit);

      --  LRM93 10.1 Declarative Region
      --  4. A package declaration, together with the corresponding
      --     body (if any).
      Open_Declarative_Region;

      Sem_Scopes.Add_Package_Declarations (Package_Decl);

      Sem_Declaration_Chain (Decl, False);
      Check_Full_Declaration (Decl, Decl);
      Check_Full_Declaration (Package_Decl, Decl);

      Close_Declarative_Region;
   end Sem_Package_Body;

   --  LRM 10.4  Use Clauses.
   procedure Sem_Use_Clause (Clauses: Iir_Use_Clause)
   is
      Clause : Iir_Use_Clause;
      Name: Iir;
      Prefix: Iir;
      Prefix_Name : Iir;
   begin
      Clause := Clauses;
      loop
         --  LRM93 10.4
         --  A use clause achieves direct visibility of declarations that are
         --  visible by selection.
         --  Each selected name is a use clause identifies one or more
         --  declarations that will potentialy become directly visible.

         Name := Get_Selected_Name (Clause);
         case Get_Kind (Name) is
            when Iir_Kind_Selected_By_All_Name
              | Iir_Kind_Selected_Name =>
               Prefix := Get_Prefix (Name);
            when others =>
               Error_Msg_Sem ("use clause allows only selected name", Name);
               return;
         end case;

         Sem_Name (Prefix, False);
         Prefix_Name := Get_Named_Entity (Prefix);
         if Prefix_Name = Error_Mark then
            return;
         end if;

         --  LRM 10.4 Use Clauses

         --  If the suffix of the selected name is [...], then the
         --  selected name identifies only the declaration(s) of that
         --  [...] contained within the package or library denoted by
         --  the prefix of the selected name.
         --
         --  If the suffix is the reserved word ALL, then the selected name
         --  identifies all declarations that are contained within the package
         --  or library denoted by the prefix of the selected name.
         --
         --  GHDL: therefore, the suffix must be either a package or a library.
         case Get_Kind (Prefix_Name) is
            when Iir_Kind_Library_Declaration =>
               null;
            when Iir_Kind_Design_Unit =>
               if Get_Kind (Get_Library_Unit (Prefix_Name))
                 /= Iir_Kind_Package_Declaration
               then
                  Error_Msg_Sem ("design unit is not a package", Prefix);
                  return;
               end if;
               Libraries.Load_Design_Unit (Prefix_Name, Clause);
            when others =>
               Error_Msg_Sem ("prefix must designate a package or a library",
                              Prefix);
               return;
         end case;

         case Get_Kind (Name) is
            when Iir_Kind_Selected_Name =>
               Sem_Name (Name, False);
               if Get_Named_Entity (Name) = Error_Mark then
                  return;
               end if;
               Xref_Name (Name);
            when Iir_Kind_Selected_By_All_Name =>
               Xref_Name (Prefix);
               null;
            when others =>
               raise Internal_Error;
         end case;

         Clause := Get_Use_Clause_Chain (Clause);
         exit when Clause = Null_Iir;
      end loop;

      --  LRM 10.4
      --  For each use clause, there is a certain region of text called the
      --  scope of the use clause.  This region starts immediatly after the
      --  use clause.
      Sem_Scopes.Add_Use_Clause (Clauses);
   end Sem_Use_Clause;

   --  LRM 11.2  Design Libraries.
   procedure Sem_Library_Clause (Decl: Iir_Library_Clause)
   is
      Ident : Name_Id;
      Lib: Iir;
   begin
      --  GHDL: 'redeclaration' is handled in sem_scopes.

      Ident := Get_Identifier (Decl);
      Lib := Libraries.Get_Library (Ident, Get_Location (Decl));
      if Lib = Null_Iir then
         Error_Msg_Sem
           ("no resource library """ & Name_Table.Image (Ident) & """", Decl);
      else
         Set_Library_Declaration (Decl, Lib);
         Sem_Scopes.Add_Name (Lib, Ident, False);
         Set_Visible_Flag (Lib, True);
         Xref_Ref (Decl, Lib);
      end if;
   end Sem_Library_Clause;

   --  LRM 11.3  Context Clauses.
   procedure Sem_Context_Clauses (Design_Unit: Iir_Design_Unit)
   is
      El: Iir;
   begin
      El := Get_Context_Items (Design_Unit);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Use_Clause =>
               Sem_Use_Clause (El);
            when Iir_Kind_Library_Clause =>
               Sem_Library_Clause (El);
            when others =>
               Error_Kind ("sem_context_clauses", El);
         end case;
         El := Get_Chain (El);
      end loop;
   end Sem_Context_Clauses;

   -- Access to the current design unit.  This is set, saved, restored, cleared
   -- by the procedure semantic.
   Current_Design_Unit: Iir_Design_Unit := Null_Iir;

   function Get_Current_Design_Unit return Iir_Design_Unit is
   begin
      return Current_Design_Unit;
   end Get_Current_Design_Unit;

   --  LRM 11.1  Design units.
   procedure Semantic (Design_Unit: Iir_Design_Unit)
   is
      El: Iir;
      Old_Design_Unit: Iir_Design_Unit;
      Implicit : Implicit_Signal_Declaration_Type;
   begin
      --  Sanity check: can analyze either previously analyzed unit or just
      --  parsed unit.
      case Get_Date (Design_Unit) is
         when Date_Parsed =>
            Set_Date (Design_Unit, Date_Analyzing);
         when Date_Valid =>
            null;
         when others =>
            raise Internal_Error;
      end case;

      -- Save and set current_design_unit.
      Old_Design_Unit := Current_Design_Unit;
      Current_Design_Unit := Design_Unit;
      Push_Signals_Declarative_Part (Implicit, Null_Iir);

      --  Be sure the name table is empty.
      --  It is empty at start-up, or saved before recursing.
      pragma Debug (Name_Table.Assert_No_Infos);

      --  LRM02 10.1 Declarative Region.
      --  In addition to the above declarative region, there is a root
      --  declarative region, not associated with a portion of the text of the
      --  description, but encompassing any given primary unit.  At the
      --  beginning of the analysis of a given primary unit, there are no
      --  declarations whose scopes (see 10.2) are within the root declarative
      --  region.  Moreover, the root declarative region associated with any
      --  given secondary unit is the root declarative region of the
      --  corresponding primary unit.
      --  GHDL: for any revision of VHDL, a root declarative region is created,
      --    due to reasons given by LCS 3 (VHDL Issue # 1028).
      Open_Declarative_Region;

      -- Set_Dependence_List (Design_Unit,
--                            Create_Iir (Iir_Kind_Design_Unit_List));

      --  LRM 11.2
      --  Every design unit is assumed to contain the following implicit
      --  context items as part of its context clause:
      --    library STD, WORK; use STD.STANDARD.all;
      Sem_Scopes.Add_Name (Libraries.Std_Library, Std_Names.Name_Std, False);
      Sem_Scopes.Add_Name (Get_Library (Get_Design_File (Design_Unit)),
                           Std_Names.Name_Work,
                           False);
      Sem_Scopes.Use_All_Names (Std_Standard_Unit);
      if Get_Dependence_List (Design_Unit) = Null_Iir_List then
         Set_Dependence_List (Design_Unit, Create_Iir_List);
      end if;
      Add_Dependence (Std_Standard_Unit);

      -- Semantic on context clauses.
      Sem_Context_Clauses (Design_Unit);

      -- semantic on the library unit.
      El := Get_Library_Unit (Design_Unit);
      case Get_Kind (El) is
         when Iir_Kind_Entity_Declaration =>
            Sem_Entity_Declaration (El);
         when Iir_Kind_Architecture_Declaration =>
            Sem_Architecture_Declaration (El);
         when Iir_Kind_Package_Declaration =>
            Sem_Package_Declaration (El);
         when Iir_Kind_Package_Body =>
            Sem_Package_Body (El);
         when Iir_Kind_Configuration_Declaration =>
            Sem_Configuration_Declaration (El);
         when others =>
            Error_Kind ("semantic", El);
      end case;

      Close_Declarative_Region;

      if Get_Date (Design_Unit) = Date_Analyzing then
         Set_Date (Design_Unit, Date_Analyzed);
      end if;

      if Get_Analysis_Checks_List (Design_Unit) /= Null_Iir_List then
         Sem_Analysis_Checks_List (Design_Unit, False);
      end if;

      -- Restore current_design_unit.
      Current_Design_Unit := Old_Design_Unit;
      Pop_Signals_Declarative_Part (Implicit);
   end Semantic;
end Sem;
