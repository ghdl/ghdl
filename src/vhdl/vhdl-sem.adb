--  Semantic analysis pass.
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

with Errorout; use Errorout;
with Libraries;
with Std_Names;
with Flags; use Flags;
with Str_Table;

with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Std_Package; use Vhdl.Std_Package;
with Vhdl.Ieee.Std_Logic_1164;
with Vhdl.Sem_Scopes; use Vhdl.Sem_Scopes;
with Vhdl.Sem_Expr; use Vhdl.Sem_Expr;
with Vhdl.Sem_Names; use Vhdl.Sem_Names;
with Vhdl.Sem_Specs; use Vhdl.Sem_Specs;
with Vhdl.Sem_Decls; use Vhdl.Sem_Decls;
with Vhdl.Sem_Assocs; use Vhdl.Sem_Assocs;
with Vhdl.Sem_Types;
with Vhdl.Sem_Inst;
with Vhdl.Sem_Lib; use Vhdl.Sem_Lib;
with Vhdl.Sem_Psl;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Sem_Utils;
with Vhdl.Sem_Stmts; use Vhdl.Sem_Stmts;
with Vhdl.Nodes_Utils;
with Vhdl.Xrefs; use Vhdl.Xrefs;
with Vhdl.Elocations;

package body Vhdl.Sem is
   -- Forward declarations.
   procedure Sem_Context_Clauses (Unit: Iir);
   procedure Sem_Block_Configuration
     (Block_Conf : Iir_Block_Configuration; Father: Iir);
   procedure Sem_Component_Configuration
     (Conf : Iir_Component_Configuration; Father : Iir);

   procedure Add_Dependence (Unit : Iir)
   is
      Targ : constant Iir := Get_Current_Design_Unit;
   begin
      --  During normal analysis, there is a current design unit.  But not
      --  during debugging outside of any context.
      if Targ = Null_Iir then
         return;
      end if;

      Add_Dependence (Targ, Unit);
   end Add_Dependence;

   --  LRM 1.1  Entity declaration.
   procedure Sem_Entity_Declaration (Entity : Iir_Entity_Declaration) is
   begin
      Xrefs.Xref_Decl (Entity);
      Sem_Scopes.Add_Name (Entity);
      Set_Visible_Flag (Entity, True);

      Set_Is_Within_Flag (Entity, True);

      --  LRM 10.1
      --  1.  An entity declaration, together with a corresponding architecture
      --      body.
      Open_Declarative_Region;

      -- Sem generics.
      Sem_Interface_Chain (Get_Generic_Chain (Entity), Generic_Interface_List);

      -- Sem ports.
      Sem_Interface_Chain (Get_Port_Chain (Entity), Port_Interface_List);

      --  Entity declarative part and concurrent statements.
      Sem_Block (Entity);

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
      Entity : Iir;
   begin
      --  Get the library of architecture/configuration.
      Library := Get_Library
        (Get_Design_File (Get_Design_Unit (Library_Unit)));

      --  Resolve the name.

      Name := Get_Entity_Name (Library_Unit);
      if Is_Error (Name) then
         pragma Assert (Flags.Flag_Force_Analysis);
         return Null_Iir;
      end if;

      if Get_Kind (Name) = Iir_Kind_Simple_Name then
         --  LRM93 10.1 Declarative Region
         --  LRM08 12.1 Declarative Region
         --  a) An entity declaration, tohether with a corresponding
         --     architecture body.
         --
         --  GHDL: simple name needs to be handled specially.  Because
         --  architecture body is in the declarative region of its entity,
         --  the entity name is directly visible.  But we cannot really use
         --  that rule as is, as we don't know which is the entity.
         Entity := Load_Primary_Unit
           (Library, Get_Identifier (Name), Library_Unit);
         if Entity = Null_Iir then
            Error_Msg_Sem (+Name, "entity %n was not analysed", +Name);
            return Null_Iir;
         end if;
         Entity := Get_Library_Unit (Entity);
         Set_Named_Entity (Name, Entity);
         Xrefs.Xref_Ref (Name, Entity);
      else
         --  Certainly an expanded name.  Use the standard name analysis.
         Name := Sem_Denoting_Name (Name);
         Set_Entity_Name (Library_Unit, Name);
         Entity := Get_Named_Entity (Name);
      end if;

      if Get_Kind (Entity) /= Iir_Kind_Entity_Declaration then
         Error_Class_Match (Name, "entity");
         return Null_Iir;
      end if;

      --  LRM 1.2 Architecture bodies
      --  For a given design entity, both the entity declaration and the
      --  associated architecture body must reside in the same library.

      --  LRM 1.3 Configuration Declarations
      --  For a configuration of a given design entity, both the
      --  configuration declaration and the corresponding entity
      --  declaration must reside in the same library.
      if Get_Library (Get_Design_File (Get_Design_Unit (Entity))) /= Library
      then
         Error_Msg_Sem
           (+Library_Unit, "%n does not reside in %n", (+Entity, +Library));
         return Null_Iir;
      end if;

      return Entity;
   end Sem_Entity_Name;

   --  LRM 1.2  Architecture bodies.
   procedure Sem_Architecture_Body (Arch: Iir_Architecture_Body)
   is
      Entity_Unit : Iir_Design_Unit;
      Entity_Library : Iir_Entity_Declaration;
   begin
      Xrefs.Xref_Decl (Arch);
      -- First, find the entity.
      Entity_Library := Sem_Entity_Name (Arch);
      if Entity_Library = Null_Iir then
         return;
      end if;
      Entity_Unit := Get_Design_Unit (Entity_Library);

      --  LRM93 11.4
      --   In each case, the second unit depends on the first unit.
      --  GHDL: an architecture depends on its entity.
      Add_Dependence (Entity_Unit);

      Add_Context_Clauses (Entity_Unit);

      Set_Is_Within_Flag (Arch, True);
      Set_Is_Within_Flag (Entity_Library, True);

      --  Makes the entity name visible.
      --  FIXME: quote LRM.
      declare
         Prev_Hide : constant Boolean := Is_Warning_Enabled (Warnid_Hide);
      begin
         --  Avoid spurious warning from entity name (if it has the same
         --   identifier as a library clause).
         Enable_Warning (Warnid_Hide, False);
         Sem_Scopes.Add_Name
           (Entity_Library, Get_Identifier (Entity_Library), False);
         Enable_Warning (Warnid_Hide, Prev_Hide);
      end;

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
      Sem_Scopes.Add_Name (Arch, Get_Identifier (Arch), True);
      Set_Visible_Flag (Arch, True);

      --  LRM02 10.1  Declarative region
      --  The declarative region associated with an architecture body is
      --  considered to occur immediatly within the declarative region
      --  associated with the entity declaration corresponding to the given
      --  architecture body.
      --
      --  GHDL: this is only in vhdl-2002.
      if Vhdl_Std = Vhdl_02 then
         Open_Declarative_Region;
      end if;

      Current_Psl_Default_Clock := Null_Iir;
      Sem_Block (Arch);

      if Vhdl_Std = Vhdl_02 then
         Close_Declarative_Region;
      end if;

      Close_Declarative_Region;
      Set_Is_Within_Flag (Arch, False);
      Set_Is_Within_Flag (Entity_Library, False);
   end Sem_Architecture_Body;

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
           | Iir_Kind_Interface_Signal_Declaration
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
         return Get_Resolution_Indication (Obj_Type);
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
      if Get_Actual_Conversion (Assoc) /= Null_Iir
        or else Get_Formal_Conversion (Assoc) /= Null_Iir
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

      Formal_Base := Get_Object_Prefix (Formal);
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
      if (Get_Guarded_Signal_Flag (Formal_Base)
            /= Get_Guarded_Signal_Flag (Actual_Base))
        or else (Get_Signal_Kind (Formal_Base)
                   /= Get_Signal_Kind (Actual_Base))
      then
         return False;
      end if;

      return True;
   end Can_Collapse_Signals;

   --  INTER_PARENT contains generics interfaces;
   --  ASSOC_PARENT constains generic aspects.
   function Sem_Generic_Association_Chain
     (Inter_Parent : Iir; Assoc_Parent : Iir) return Boolean
   is
      El : Iir;
      Match : Compatibility_Level;
      Assoc_Chain : Iir;
      Inter_Chain : Iir;
      Miss : Missing_Type;
   begin
      --  LRM08 6.5.6.2 Generic clauses
      --  If no such actual is specified for a given formal generic constant
      --  (either because the formal generic is unassociated or because the
      --  actual is open), and if a default expression is specified for that
      --  generic, the value of this expression is the value of the generic.
      --  It is an error if no actual is specified for a given formal generic
      --  constant and no default expression is present in the corresponding
      --  interface element.

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

            --  GHDL: for a direct instantiation, follow rules of
            --  LRM 1.1.1.1 Generic and LRM 1.1.1.2 Ports.
            --  The difference between 87 and 93 is simply a clarification:
            --  missing association are left open, but need a default
            --  expression in the formal declaration.
            Miss := Missing_Generic;
         when Iir_Kind_Binding_Indication =>
            --  LRM 5.2.1.2  Generic map and port map aspects
            Miss := Missing_Allowed;
         when Iir_Kind_Block_Header =>
            Miss := Missing_Generic;
         when Iir_Kind_Procedure_Instantiation_Declaration
            | Iir_Kind_Function_Instantiation_Declaration =>
            --  LRM08 4.4
            --  Each formal generic (or member thereof) shall be associated
            --  at most once.
            Miss := Missing_Generic;
         when Iir_Kind_Package_Instantiation_Declaration
            | Iir_Kind_Interface_Package_Declaration
            | Iir_Kind_Package_Header =>
            --  LRM08 4.9
            --  Each formal generic (or member thereof) shall be associated
            --  at most once.
            Miss := Missing_Generic;
         when others =>
            Error_Kind ("sem_generic_association_list", Assoc_Parent);
      end case;

      --  The generics
      Inter_Chain := Get_Generic_Chain (Inter_Parent);
      Assoc_Chain := Get_Generic_Map_Aspect_Chain (Assoc_Parent);

      --  Extract non-object associations, as the actual cannot be analyzed
      --  as an expression.
      Assoc_Chain := Extract_Non_Object_Association (Assoc_Chain, Inter_Chain);
      Set_Generic_Map_Aspect_Chain (Assoc_Parent, Assoc_Chain);

      if not Sem_Actual_Of_Association_Chain (Assoc_Chain) then
         return False;
      end if;

      Sem_Association_Chain
        (Inter_Chain, Assoc_Chain, True, Miss, Assoc_Parent, Match);
      Set_Generic_Map_Aspect_Chain (Assoc_Parent, Assoc_Chain);
      if Match = Not_Compatible then
         return False;
      end if;

      --  LRM 5.2.1.2   Generic map and port map aspects
      --  An actual associated with a formal generic map aspect must be an
      --  expression or the reserved word open;
      El := Assoc_Chain;
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Association_Element_By_Expression =>
               Check_Read (Get_Actual (El));
            when Iir_Kind_Association_Element_Open
              | Iir_Kind_Association_Element_By_Individual
              | Iir_Kind_Association_Element_Package
              | Iir_Kind_Association_Element_Type
              | Iir_Kind_Association_Element_Subprogram =>
               null;
            when others =>
               Error_Kind ("sem_generic_association_chain(1)", El);
         end case;
         El := Get_Chain (El);
      end loop;

      return True;
   end Sem_Generic_Association_Chain;

   procedure Sem_Generic_Association_Chain
     (Inter_Parent : Iir; Assoc_Parent : Iir)
   is
      Res : Boolean;
      pragma Unreferenced (Res);
   begin
      Res := Sem_Generic_Association_Chain (Inter_Parent, Assoc_Parent);
   end Sem_Generic_Association_Chain;

   function Sem_Signal_Port_Association
     (Assoc : Iir; Formal : Iir; Formal_Base : Iir) return Iir
   is
      Actual : Iir;
      N_Assoc : Iir;
      Prefix : Iir;
      Object : Iir;
   begin
      Actual := Get_Actual (Assoc);
      --  There has been an error, return now.
      if Actual = Null_Iir then
         return Assoc;
      end if;
      Object := Name_To_Object (Actual);

      if Is_Valid (Object) and then Is_Signal_Object (Object) then
         --  Port or signal.

         --  Mutate to By_Name.
         N_Assoc := Create_Iir (Iir_Kind_Association_Element_By_Name);
         Location_Copy (N_Assoc, Assoc);
         Set_Formal (N_Assoc, Get_Formal (Assoc));
         Set_Chain (N_Assoc, Get_Chain (Assoc));
         Set_Actual (N_Assoc, Actual);
         Set_Actual_Conversion (N_Assoc, Get_Actual_Conversion (Assoc));
         Set_Formal_Conversion (N_Assoc, Get_Formal_Conversion (Assoc));
         Set_Whole_Association_Flag
           (N_Assoc, Get_Whole_Association_Flag (Assoc));
         pragma Assert (not Get_In_Formal_Flag (Assoc));
         if Flag_Elocations then
            declare
               use Vhdl.Elocations;
            begin
               Create_Elocations (N_Assoc);
               Set_Arrow_Location (N_Assoc, Get_Arrow_Location (Assoc));
            end;
         end if;
         Free_Iir (Assoc);

         Set_Collapse_Signal_Flag
           (N_Assoc, Can_Collapse_Signals (N_Assoc, Formal));
         if Get_Name_Staticness (Object) < Globally then
            Error_Msg_Sem (+Actual, "actual must be a static name");
         end if;
         Check_Port_Association_Bounds_Restrictions
           (Formal, Actual, N_Assoc);
         Prefix := Get_Object_Prefix (Object);
         case Get_Kind (Prefix) is
            when Iir_Kind_Interface_Signal_Declaration =>
               declare
                  P : Boolean;
                  pragma Unreferenced (P);
               begin
                  P := Check_Port_Association_Mode_Restrictions
                    (Formal_Base, Prefix, N_Assoc);
               end;
            when Iir_Kind_Signal_Declaration =>
               Set_Use_Flag (Prefix, True);
            when others =>
               --  FIXME: attributes ?
               null;
         end case;
         return N_Assoc;
      else
         --  Expression.
         Set_Collapse_Signal_Flag (Assoc, False);

         pragma Assert (Is_Null (Get_Actual_Conversion (Assoc)));
         if Flags.Vhdl_Std >= Vhdl_93 then
            --  LRM93 1.1.1.2 Ports
            --  Moreover, the ports of a block may be associated
            --  with an expression, in order to provide these ports
            --  with constant driving values; such ports must be
            --  of mode in.
            if Get_Mode (Formal_Base) /= Iir_In_Mode then
               Error_Msg_Sem
                 (+Assoc, "only 'in' ports may be associated with expression");
            end if;

            --  Is it possible to have a globally static name that is
            --  not readable ?
            Check_Read (Actual);

            --  LRM93 1.1.1.2 Ports
            --  The actual, if an expression, must be a globally
            --  static expression.
            if Get_Expr_Staticness (Actual) < Globally then
               if Flags.Vhdl_Std < Vhdl_08 then
                  --  LRM08 6.5.6.3 Port clauses
                  Error_Msg_Sem
                    (+Actual, "actual expression must be globally static");
               end if;
            end if;
         else
            Error_Msg_Sem
              (+Assoc, "cannot associate ports with expression in vhdl87");
         end if;
         return Assoc;
      end if;
   end Sem_Signal_Port_Association;

   --  INTER_PARENT contains ports interfaces;
   --  ASSOC_PARENT constains ports map aspects.
   procedure Sem_Port_Association_Chain
     (Inter_Parent : Iir; Assoc_Parent : Iir)
   is
      Assoc : Iir;
      Prev_Assoc, N_Assoc : Iir;
      Match : Compatibility_Level;
      Assoc_Chain : Iir;
      Inter_Chain : Iir;
      Miss : Missing_Type;
      Inter : Iir;
      Formal : Iir;
      Formal_Base : Iir;
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

            --  GHDL: for a direct instantiation, follow rules of
            --  LRM 1.1.1.1 Generic and LRM 1.1.1.2 Ports.
            Miss := Missing_Port;
         when Iir_Kind_Binding_Indication =>
            --  LRM 5.2.1.2  Generic map and port map aspects
            Miss := Missing_Allowed;
         when Iir_Kind_Block_Header =>
            --  FIXME: it is possible to have port unassociated ?
            Miss := Missing_Port;
         when others =>
            Error_Kind ("sem_port_association_list", Assoc_Parent);
      end case;

      --  The ports
      Assoc_Chain := Get_Port_Map_Aspect_Chain (Assoc_Parent);
      Inter_Chain := Get_Port_Chain (Inter_Parent);

      if AMS_Vhdl then
         --  Mutate terminal associations, so that their formals are not
         --  analyzed as an expression.
         Assoc_Chain :=
           Extract_Non_Object_Association (Assoc_Chain, Inter_Chain);
      end if;

      if not Sem_Actual_Of_Association_Chain (Assoc_Chain) then
         return;
      end if;
      Sem_Association_Chain (Inter_Chain, Assoc_Chain,
                             True, Miss, Assoc_Parent, Match);
      Set_Port_Map_Aspect_Chain (Assoc_Parent, Assoc_Chain);
      if Match = Not_Compatible then
         --  TODO: mark actual as used to avoid warnings.
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
      Assoc := Assoc_Chain;
      Inter := Get_Port_Chain (Inter_Parent);
      Prev_Assoc := Null_Iir;
      while Assoc /= Null_Iir loop
         Formal := Get_Association_Formal (Assoc, Inter);
         Formal_Base := Get_Interface_Of_Formal (Formal);

         case Get_Kind (Formal_Base) is
            when Iir_Kind_Interface_Signal_Declaration =>
               if Get_Kind (Assoc) = Iir_Kind_Association_Element_By_Expression
               then
                  N_Assoc := Sem_Signal_Port_Association
                    (Assoc, Formal, Formal_Base);

                  --  Reinsert the new association (in case of mutation).
                  if N_Assoc /= Assoc then
                     if Prev_Assoc /= Null_Iir then
                        Set_Chain (Prev_Assoc, N_Assoc);
                     else
                        Set_Port_Map_Aspect_Chain (Assoc_Parent, N_Assoc);
                     end if;
                     Assoc := N_Assoc;
                  end if;
               end if;
            when others =>
               null;
         end case;

         Prev_Assoc := Assoc;
         Next_Association_Interface (Assoc, Inter);
      end loop;
   end Sem_Port_Association_Chain;

   --  INTER_PARENT contains generics and ports interfaces;
   --  ASSOC_PARENT constains generics and ports map aspects.
   procedure Sem_Generic_Port_Association_Chain
     (Inter_Parent : Iir; Assoc_Parent : Iir)
   is
      Res : Boolean;
      pragma Unreferenced (Res);
   begin
      Sem_Generic_Association_Chain (Inter_Parent, Assoc_Parent);
      Sem_Port_Association_Chain (Inter_Parent, Assoc_Parent);
   end Sem_Generic_Port_Association_Chain;

   --  LRM 1.3  Configuration Declarations.
   procedure Sem_Configuration_Declaration (Decl: Iir)
   is
      Entity: Iir_Entity_Declaration;
      Entity_Unit : Iir_Design_Unit;
   begin
      Xref_Decl (Decl);
      Set_Is_Within_Flag (Decl, True);

      --  LRM 1.3
      --  The entity name identifies the name of the entity declaration that
      --  defines the design entity at the apex of the design hierarchy.
      Entity := Sem_Entity_Name (Decl);
      if Entity = Null_Iir then
         return;
      end if;
      Entity_Unit := Get_Design_Unit (Entity);

      --  LRM 11.4
      --  A primary unit whose name is referenced within a given design unit
      --  must be analyzed prior to the analysis of the given design unit.
      Add_Dependence (Entity_Unit);

      Sem_Scopes.Add_Name (Decl);

      Set_Visible_Flag (Decl, True);

      --  LRM 10.1 Declarative Region
      --  2.  A configuration declaration.
      Open_Declarative_Region;

      --  LRM93 10.2
      --  In addition to the above rules, the scope of any declaration that
      --  includes the end of the declarative part of a given block (whether
      --  it be an external block defined by a design entity or an internal
      --  block defined by a block statement) extends into a configuration
      --  declaration that configures the given block.
      Add_Context_Clauses (Entity_Unit);
      Sem_Scopes.Add_Entity_Declarations (Entity);

      Sem_Declaration_Chain (Decl);
      --  GHDL: no need to check for missing subprogram bodies, since they are
      --  not allowed in configuration declarations.

      Sem_Block_Configuration (Get_Block_Configuration (Decl), Decl);
      Close_Declarative_Region;
      Set_Is_Within_Flag (Decl, False);
   end Sem_Configuration_Declaration;

   --  Analyze the block specification of a block statement or of a generate
   --  statement.  Return the corresponding block statement, generate
   --  statement body, or Null_Iir in case of error.
   function Sem_Block_Specification_Of_Statement
     (Block_Conf : Iir_Block_Configuration; Father : Iir) return Iir
   is
      Block_Spec : Iir;
      Block_Name : Iir;
      Block_Stmts : Iir;
      Prev : Iir_Block_Configuration;
      Block : Iir;
      Res : Iir;
      Assoc : Iir;
      Clause : Iir;
      Gen_Spec : Iir;
   begin
      Block_Spec := Get_Block_Specification (Block_Conf);
      case Get_Kind (Block_Spec) is
         when Iir_Kind_Simple_Name =>
            Block_Name := Block_Spec;
         when Iir_Kind_Parenthesis_Name
           | Iir_Kind_Slice_Name =>
            Block_Name := Get_Prefix (Block_Spec);
         when others =>
            Error_Msg_Sem (+Block_Spec, "label expected");
            return Null_Iir;
      end case;

      --  Analyze the label and generate specification.
      Block_Name := Sem_Denoting_Name (Block_Name);
      Block := Get_Named_Entity (Block_Name);
      case Get_Kind (Block) is
         when Iir_Kind_Block_Statement =>
            if Get_Kind (Block_Spec) /= Iir_Kind_Simple_Name then
               Error_Msg_Sem (+Block_Spec,
                              "label does not denote a generate statement");
            end if;
            Set_Block_Specification (Block_Conf, Block_Name);
            Prev := Get_Block_Block_Configuration (Block);
            Res := Block;

         when Iir_Kind_For_Generate_Statement =>
            Res := Get_Generate_Statement_Body (Block);
            Set_Named_Entity (Block_Name, Res);
            Prev := Get_Generate_Block_Configuration (Res);

            case Get_Kind (Block_Spec) is
               when Iir_Kind_Simple_Name =>
                  Set_Block_Specification (Block_Conf, Block_Name);
               when Iir_Kind_Parenthesis_Name =>
                  Block_Spec := Sem_Index_Specification
                    (Block_Spec,
                     Get_Type (Get_Parameter_Specification (Block)));
                  if Block_Spec /= Null_Iir then
                     Set_Prefix (Block_Spec, Block_Name);
                     Set_Block_Specification (Block_Conf, Block_Spec);
                  end if;
               when others =>
                  raise Internal_Error;
            end case;

         when Iir_Kind_If_Generate_Statement =>
            case Get_Kind (Block_Spec) is
               when Iir_Kind_Simple_Name =>
                  --  LRM08 3.4.2 Block configuration
                  --  If no generate specification appears in such a block
                  --  configuration, then it applies to exactly one of the
                  --  following sets of blocks:
                  --  [...]
                  --  - The implicit block generated by the corresponding
                  --    generate statement, if and only if the corresponding
                  --    generate is an if generate statement and if the first
                  --    condition after IF evaluates to TRUE.
                  Res := Get_Generate_Statement_Body (Block);

                  --  LRM08 3.4.2 Block configuration
                  --  If the block specification of a block configuration
                  --  contains a generate statement label that denotes an if
                  --  generate statement, and if the first condition after IF
                  --  has an alternative label, then it is an error if the
                  --  generate statement label does not contain a generate
                  --  specification that is an alternative label.
                  if Get_Has_Label (Res) then
                     Error_Msg_Sem
                       (+Block_Spec,
                        "alternative label required in block specification");
                  end if;

                  Set_Block_Specification (Block_Conf, Block_Name);

               when Iir_Kind_Parenthesis_Name =>
                  if Vhdl_Std < Vhdl_08 then
                     Error_Msg_Sem
                       (+Block_Spec,
                        "alternative label only allowed by vhdl08");
                     return Null_Iir;
                  end if;
                  Assoc := Get_Association_Chain (Block_Spec);
                  pragma Assert
                    (Get_Kind (Assoc)
                       = Iir_Kind_Association_Element_By_Expression);
                  Gen_Spec := Get_Actual (Assoc);
                  if Get_Kind (Gen_Spec) /= Iir_Kind_Simple_Name then
                     Error_Msg_Sem
                       (+Gen_Spec,
                        "alternative label expected for if-generate");
                     return Null_Iir;
                  end if;
                  --  Search label.
                  Clause := Block;
                  while Clause /= Null_Iir loop
                     Res := Get_Generate_Statement_Body (Clause);
                     exit when Get_Alternative_Label (Res)
                       = Get_Identifier (Gen_Spec);
                     Clause := Get_Generate_Else_Clause (Clause);
                  end loop;
                  if Clause = Null_Iir then
                     Error_Msg_Sem
                       (+Gen_Spec,
                        "alternative label %i not found for if-generate",
                        +Gen_Spec);
                     return Null_Iir;
                  end if;
                  Set_Named_Entity (Block_Spec, Res);
                  Xref_Ref (Gen_Spec, Res);
                  Set_Prefix (Block_Spec, Block_Name);
                  Set_Block_Specification (Block_Conf, Block_Spec);

               when others =>
                  raise Internal_Error;
            end case;

            Set_Named_Entity (Block_Name, Res);
            Prev := Get_Generate_Block_Configuration (Res);

         when Iir_Kind_Case_Generate_Statement =>
            case Get_Kind (Block_Spec) is
               when Iir_Kind_Simple_Name =>
                  --  LRM08 3.4.2 Block configuration
                  --  If no generate specification appears in such a block
                  --  configuration, [...]
                  --  GHDL: doesn't apply to case generate statement
                  Error_Msg_Sem
                    (+Block_Spec,
                     "missing alternative label for a case-generate");
                  return Null_Iir;
               when Iir_Kind_Parenthesis_Name =>
                  Assoc := Get_Association_Chain (Block_Spec);
                  pragma Assert
                    (Get_Kind (Assoc)
                       = Iir_Kind_Association_Element_By_Expression);
                  Gen_Spec := Get_Actual (Assoc);
                  if Get_Kind (Gen_Spec) /= Iir_Kind_Simple_Name then
                     Error_Msg_Sem
                       (+Gen_Spec,
                        "alternative label expected for case-generate");
                     return Null_Iir;
                  end if;
                  --  Search label.
                  Clause := Get_Case_Statement_Alternative_Chain (Block);
                  while Clause /= Null_Iir loop
                     Res := Get_Associated_Block (Clause);
                     exit when Get_Alternative_Label (Res)
                       = Get_Identifier (Gen_Spec);
                     Clause := Get_Chain (Clause);
                  end loop;
                  if Clause = Null_Iir then
                     Error_Msg_Sem
                       (+Gen_Spec,
                        "alternative label %i not found for case-generate",
                        +Gen_Spec);
                     return Null_Iir;
                  end if;
                  Set_Named_Entity (Block_Spec, Res);
                  Xref_Ref (Gen_Spec, Res);
                  Set_Prefix (Block_Spec, Block_Name);
                  Set_Block_Specification (Block_Conf, Block_Spec);

               when others =>
                  raise Internal_Error;
            end case;

            Set_Named_Entity (Block_Name, Res);
            Prev := Get_Generate_Block_Configuration (Res);

         when others =>
            Error_Msg_Sem (+Block_Conf,
                           "block or generate statement label expected");
            return Null_Iir;
      end case;

      --  LRM93 1.3.1 / LRM08 3.4.2 Block configuration
      --  [...], and the label must denote a block statement or generate
      --  statement that is contained immediatly within the block denoted by
      --  the block specification of the containing block configuration.
      Block_Stmts := Get_Concurrent_Statement_Chain
        (Get_Block_From_Block_Specification
           (Get_Block_Specification (Father)));
      if not Is_In_Chain (Block_Stmts, Block) then
         Error_Msg_Sem (+Block_Conf,
                        "label does not denotes an inner block statement");
         return Null_Iir;
      end if;

      case Get_Kind (Block) is
         when Iir_Kind_Block_Statement =>
            --  LRM93 1.3
            --  It is an error if, in a given block configuration, more than
            --  one configuration item is defined for the same block [or
            --  component instance].
            if Prev /= Null_Iir then
               Error_Msg_Sem
                 (+Block_Conf,
                  "%n was already configured at %l", (+Block, +Prev));
               return Null_Iir;
            end if;
            Set_Block_Block_Configuration (Res, Block_Conf);

         when Iir_Kind_If_Generate_Statement
           | Iir_Kind_Case_Generate_Statement =>
            --  LRM93 1.3
            --  It is an error if, in a given block configuration, more than
            --  one configuration item is defined for the same block [or
            --  component instance].
            if Prev /= Null_Iir then
               Error_Msg_Sem
                 (+Block_Conf,
                  "%n was already configured at %l", (+Block, +Prev));
               return Null_Iir;
            end if;
            Set_Generate_Block_Configuration (Res, Block_Conf);

         when Iir_Kind_For_Generate_Statement =>
            --  LRM93 1.3
            --  For any name that is the label of a generate statement
            --  immediately wihin a given block, one or more corresponding
            --  block configuration may appear as configuration items
            --  immediately within a block configuration corresponding to the
            --  given block.
            --  GHDL: keep them in a linked list, but don't try to detect
            --  duplicate as values may not be static.  FIXME: try for
            --  static values only ?
            Set_Prev_Block_Configuration (Block_Conf, Prev);
            Set_Generate_Block_Configuration (Res, Block_Conf);
         when others =>
            raise Internal_Error;
      end case;
      return Res;
   end Sem_Block_Specification_Of_Statement;

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
               Arch : Iir_Architecture_Body;
               Design: Iir_Design_Unit;
            begin
               Block_Spec := Get_Block_Specification (Block_Conf);
               --  FIXME: handle selected name.
               if Get_Kind (Block_Spec) /= Iir_Kind_Simple_Name then
                  Error_Msg_Sem (+Block_Spec, "architecture name expected");
                  return;
               end if;
               --  LRM 10.3 rule b)
               --  For an architecture body associated with a given entity
               --  declaration: at the place of the block specification in a
               --  block configuration for an external block whose interface
               --  is defined by that entity declaration.
               Design := Load_Secondary_Unit
                 (Get_Design_Unit (Get_Entity (Father)),
                  Get_Identifier (Block_Spec),
                  Block_Conf);
               if Design = Null_Iir then
                  Error_Msg_Sem
                    (+Block_Conf, "no architecture %i", +Block_Spec);
                  return;
               end if;
               Arch := Get_Library_Unit (Design);
               Set_Named_Entity (Block_Spec, Arch);
               Xref_Ref (Block_Spec, Arch);
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
               Arch : Iir_Architecture_Body;
               Design: Iir_Design_Unit;
               Entity_Aspect : Iir;
               Entity : Iir;
               Comp_Arch : Iir;
            begin
               Entity_Aspect :=
                 Get_Entity_Aspect (Get_Binding_Indication (Father));
               if Entity_Aspect = Null_Iir or else
                 Get_Kind (Entity_Aspect) /= Iir_Kind_Entity_Aspect_Entity
               then
                  Error_Msg_Sem
                    (+Block_Conf, "corresponding component not fully bound");
               end if;

               Block_Spec := Get_Block_Specification (Block_Conf);
               --  FIXME: handle selected name.
               if Get_Kind (Block_Spec) /= Iir_Kind_Simple_Name then
                  Error_Msg_Sem (+Block_Spec, "architecture name expected");
                  return;
               end if;

               Comp_Arch := Get_Architecture (Entity_Aspect);
               if Comp_Arch /= Null_Iir then
                  pragma Assert (Get_Kind (Comp_Arch) = Iir_Kind_Simple_Name);
                  if Get_Identifier (Comp_Arch) /= Get_Identifier (Block_Spec)
                  then
                     Error_Msg_Sem
                       (+Block_Spec, "block specification name is different "
                          & "from component architecture name");
                     return;
                  end if;
               end if;

               Entity := Get_Entity (Entity_Aspect);
               if Entity = Null_Iir then
                  return;
               end if;

               Design := Load_Secondary_Unit (Get_Design_Unit (Entity),
                                              Get_Identifier (Block_Spec),
                                              Block_Conf);
               if Design = Null_Iir then
                  Error_Msg_Sem
                    (+Block_Conf, "no architecture %i", +Block_Spec);
                  return;
               end if;
               Add_Dependence (Design);
               Arch := Get_Library_Unit (Design);
               Set_Named_Entity (Block_Spec, Arch);
               Xref_Ref (Block_Spec, Arch);
               Block := Arch;
            end;

         when Iir_Kind_Block_Configuration =>
            --  LRM93 1.3.1 / LRM08 3.4.2 Block configuration
            --  If a block configuration appears immediately within another
            --  block configuration, then the block specification of the
            --  contained block configuration must be a block statement or
            --  generate statement label, and the label must denote a block
            --  statement or generate statement that is contained immediatly
            --  within the block denoted by the block specification of the
            --  containing block configuration.
            Block := Sem_Block_Specification_Of_Statement (Block_Conf, Father);
            if Block = Null_Iir then
               return;
            end if;

         when others =>
            Error_Kind ("sem_block_configuration", Father);
      end case;

      --  LRM93 10.1
      --  10. A block configuration
      Sem_Scopes.Open_Scope_Extension;

      --  LRM 10.3
      --  In addition, any declaration that is directly visible at the end of
      --  the declarative part of a given block is directly visible in a block
      --  configuration that configure the given block.  This rule holds unless
      --  a use clause that makes a homograph of the declaration potentially
      --  visible (see 10.4) appears in the corresponding configuration
      --  declaration, and if the scope of that use clause encompasses all or
      --  part of those configuration items.  If such a use clause appears,
      --  then the declaration will be directly visible within the
      --  corresponding configuration items, except at hose places that fall
      --  within the scope of the additional use clause.  At such places,
      --  neither name will be directly visible.
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
      Clear_Instantiation_Configuration (Block);

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

   --  Check that incremental binding of the component configuration CONF only
   --  rebinds non associated ports of each instantiations of CONFIGURED_BLOCK
   --  which CONF applies to.
   procedure Check_Incremental_Binding (Configured_Block : Iir; Conf : Iir)
   is
      Comp : constant Iir := Get_Named_Entity (Get_Component_Name (Conf));
      Inter_Chain : constant Iir := Get_Port_Chain (Comp);
      Binding : constant Iir := Get_Binding_Indication (Conf);
      Inst : Iir;
   begin
      --  Check each component instantiation of the block configured by CONF.
      Inst := Get_Concurrent_Statement_Chain (Configured_Block);
      while Inst /= Null_Iir loop
         if Get_Kind (Inst) = Iir_Kind_Component_Instantiation_Statement
           and then Get_Component_Configuration (Inst) = Conf
         then
            --  Check this instantiation.
            declare
               Primary_Binding : constant Iir := Get_Binding_Indication
                 (Get_Configuration_Specification (Inst));
               F_Chain : constant Iir :=
                 Get_Port_Map_Aspect_Chain (Primary_Binding);
               S_El : Iir;
               S_Inter : Iir;
               F_El : Iir;
               Formal : Iir;
            begin
               S_El := Get_Port_Map_Aspect_Chain (Binding);
               S_Inter := Inter_Chain;
               while S_El /= Null_Iir loop
                  --  Find S_EL formal in F_CHAIN.
                  Formal := Get_Association_Interface (S_El, S_Inter);
                  F_El := Find_First_Association_For_Interface
                    (F_Chain, Inter_Chain, Formal);
                  if F_El /= Null_Iir
                    and then
                    Get_Kind (F_El) /= Iir_Kind_Association_Element_Open
                  then
                     Error_Msg_Sem
                       (+S_El,
                        "%n already associated in primary binding", +Formal);
                  end if;
                  Next_Association_Interface (S_El, S_Inter);
               end loop;
            end;
         end if;
         Inst := Get_Chain (Inst);
      end loop;
   end Check_Incremental_Binding;

   --  LRM 1.3.2
   procedure Sem_Component_Configuration
     (Conf : Iir_Component_Configuration; Father : Iir)
   is
      Block : Iir;
      Configured_Block : Iir;
      Binding : Iir;
      Entity : Iir_Design_Unit;
      Comp : Iir_Component_Declaration;
      Primary_Binding : Iir;
   begin
      --  LRM 10.1 Declarative Region
      --  11. A component configuration.
      Open_Declarative_Region;

      --  LRM93 10.2
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
         pragma Assert (Get_Kind (Configured_Block) /= Iir_Kind_Design_Unit);
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
        (Configured_Block, Conf, Primary_Binding);

      Comp := Get_Named_Entity (Get_Component_Name (Conf));
      if Get_Kind (Comp) /= Iir_Kind_Component_Declaration then
         --  There has been an error in sem_component_specification.
         --  Leave here.
         Close_Declarative_Region;
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
      Open_Declarative_Region;
      Sem_Scopes.Add_Component_Declarations (Comp);
      Binding := Get_Binding_Indication (Conf);
      if Binding /= Null_Iir then
         Sem_Binding_Indication (Binding, Conf, Primary_Binding);

         if Primary_Binding /= Null_Iir then
            --  LRM93 5.2.1  Binding Indication
            --  It is an error if a formal port appears in the port map aspect
            --  of the incremental binding indication and it is a formal
            --  port that is associated with an actual other than OPEN in one
            --  of the primary binding indications.
            Check_Incremental_Binding (Configured_Block, Conf);
         end if;
      elsif Primary_Binding = Null_Iir then
         --  LRM93 5.2.1
         --  If the generic map aspect or port map aspect of a primary binding
         --  indication is not present, then the default rules as described
         --  in 5.2.2 apply.

         --  Create a default binding indication.
         Entity := Get_Visible_Entity_Declaration (Comp);
         Binding := Sem_Create_Default_Binding_Indication
           (Comp, Entity, Conf, False, False);

         if Binding /= Null_Iir then
            --  Remap to defaults.
            Set_Default_Entity_Aspect (Binding, Get_Entity_Aspect (Binding));
            Set_Entity_Aspect (Binding, Null_Iir);

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

   function Are_Trees_List_Equal (Left, Right : Iir_Flist) return Boolean
   is
      El_Left, El_Right : Iir;
   begin
      pragma Assert (Flist_Last (Left) = Flist_Last (Right));
      for I in Flist_First .. Flist_Last (Left) loop
         El_Left := Get_Nth_Element (Left, I);
         El_Right := Get_Nth_Element (Right, I);
         if not Are_Trees_Equal (El_Left, El_Right) then
            return False;
         end if;
      end loop;
      return True;
   end Are_Trees_List_Equal;

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
         when Iir_Kind_Procedure_Declaration =>
            return Are_Trees_Chain_Equal
              (Get_Interface_Declaration_Chain (Left),
               Get_Interface_Declaration_Chain (Right));
         when Iir_Kind_Function_Declaration =>
            if not Are_Trees_Equal (Get_Return_Type (Left),
                                    Get_Return_Type (Right))
            then
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
         when Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Interface_File_Declaration =>
            if Get_Identifier (Left) /= Get_Identifier (Right) then
               return False;
            end if;
            if Get_Has_Mode (Left) /= Get_Has_Mode (Right)
              or else Get_Has_Class (Left) /= Get_Has_Class (Right)
              or else (Get_Has_Identifier_List (Left)
                         /= Get_Has_Identifier_List (Right))
              or else Get_Mode (Left) /= Get_Mode (Right)
            then
               return False;
            end if;
            if not Are_Trees_Equal (Get_Subtype_Indication (Left),
                                    Get_Subtype_Indication (Right))
            then
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
            if Get_Base_Type (Left) /= Get_Base_Type (Right) then
               return False;
            end if;
            if Get_Type_Declarator (Left) /= Get_Type_Declarator (Right) then
               return False;
            end if;
            if not Are_Trees_Equal (Get_Resolution_Indication (Left),
                                    Get_Resolution_Indication (Right))
            then
               return False;
            end if;
            if Are_Trees_Equal (Get_Range_Constraint (Left),
                                Get_Range_Constraint (Right)) = False
            then
               return False;
            end if;
            return True;
         when Iir_Kind_Array_Subtype_Definition =>
            if Get_Base_Type (Left) /= Get_Base_Type (Right) then
               return False;
            end if;
            if not Are_Trees_Equal (Get_Resolution_Indication (Left),
                                    Get_Resolution_Indication (Right))
            then
               return False;
            end if;
            if not Are_Trees_List_Equal (Get_Index_Subtype_List (Left),
                                         Get_Index_Subtype_List (Right))
            then
               return False;
            end if;
            return True;
         when Iir_Kind_Record_Subtype_Definition =>
            if Get_Base_Type (Left) /= Get_Base_Type (Right) then
               return False;
            end if;
            return Are_Trees_Equal (Get_Resolution_Indication (Left),
                                    Get_Resolution_Indication (Right))
              and then
              Are_Trees_List_Equal (Get_Elements_Declaration_List (Left),
                                    Get_Elements_Declaration_List (Right));

         when Iir_Kind_Integer_Literal =>
            if Get_Value (Left) /= Get_Value (Right) then
               return False;
            end if;
            return Are_Trees_Equal (Get_Literal_Origin (Left),
                                    Get_Literal_Origin (Right));
         when Iir_Kind_Enumeration_Literal =>
            if Get_Enum_Pos (Left) /= Get_Enum_Pos (Right) then
               return False;
            end if;
            return Are_Trees_Equal (Get_Literal_Origin (Left),
                                    Get_Literal_Origin (Right));
         when Iir_Kind_Physical_Int_Literal =>
            if Get_Value (Left) /= Get_Value (Right)
              or else not Are_Trees_Equal (Get_Unit_Name (Left),
                                           Get_Unit_Name (Right))
            then
               return False;
            end if;
            return Are_Trees_Equal (Get_Literal_Origin (Left),
                                    Get_Literal_Origin (Right));
         when Iir_Kind_Physical_Fp_Literal =>
            if Get_Fp_Value (Left) /= Get_Fp_Value (Right)
              or else not Are_Trees_Equal (Get_Unit_Name (Left),
                                           Get_Unit_Name (Right))
            then
               return False;
            end if;
            return Are_Trees_Equal (Get_Literal_Origin (Left),
                                    Get_Literal_Origin (Right));
         when Iir_Kind_Unit_Declaration =>
            return Left = Right;
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

         when Iir_Kind_Function_Call =>
            return Are_Trees_Equal (Get_Prefix (Left), Get_Prefix (Right))
              and then
              Are_Trees_Chain_Equal (Get_Parameter_Association_Chain (Left),
                                     Get_Parameter_Association_Chain (Right));

         when Iir_Kind_Association_Element_By_Expression =>
            return Are_Trees_Equal (Get_Actual (Left), Get_Actual (Right))
              and then Are_Trees_Equal (Get_Formal (Left), Get_Formal (Right))
              and then Are_Trees_Equal (Get_Actual_Conversion (Left),
                                        Get_Actual_Conversion (Right))
              and then Are_Trees_Equal (Get_Formal_Conversion (Left),
                                        Get_Formal_Conversion (Right));

         when Iir_Kind_Type_Conversion =>
            return Are_Trees_Equal (Get_Type_Mark (Left),
                                    Get_Type_Mark (Right))
              and then
              Are_Trees_Equal (Get_Expression (Left),
                               Get_Expression (Right));

         when Iir_Kind_Indexed_Name =>
            return Are_Trees_Equal (Get_Prefix (Left),
                                    Get_Prefix (Right))
              and then
              Are_Trees_List_Equal (Get_Index_List (Left),
                                    Get_Index_List (Right));
         when Iir_Kind_Slice_Name =>
            return Are_Trees_Equal (Get_Prefix (Left),
                                    Get_Prefix (Right))
              and then Are_Trees_Equal (Get_Suffix (Left),
                                        Get_Suffix (Right));
         when Iir_Kind_Selected_Element =>
            return Are_Trees_Equal (Get_Prefix (Left),
                                    Get_Prefix (Right))
              and then Get_Identifier (Left) = Get_Identifier (Right);

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
            return Are_Trees_Equal (Get_Left_Limit (Left),
                                    Get_Left_Limit (Right))
              and then Are_Trees_Equal (Get_Right_Limit (Left),
                                        Get_Right_Limit (Right));

         when Iir_Kind_High_Type_Attribute
           | Iir_Kind_Low_Type_Attribute
           | Iir_Kind_Left_Type_Attribute
           | Iir_Kind_Right_Type_Attribute
           | Iir_Kind_Ascending_Type_Attribute =>
            return Are_Trees_Equal (Get_Prefix (Left), Get_Prefix (Right));

         when Iir_Kind_Length_Array_Attribute
            | Iir_Kind_Left_Array_Attribute
            | Iir_Kind_Right_Array_Attribute
            | Iir_Kind_Low_Array_Attribute
            | Iir_Kind_High_Array_Attribute
            | Iir_Kind_Ascending_Array_Attribute =>
            return Are_Trees_Equal (Get_Prefix (Left), Get_Prefix (Right))
              and then
              Are_Trees_Equal (Get_Parameter (Left), Get_Parameter (Right));

         when Iir_Kind_String_Literal8 =>
            if Get_Bit_String_Base (Left) /= Get_Bit_String_Base (Right) then
               return False;
            end if;
            declare
               use Str_Table;
               Len : constant Nat32 := Get_String_Length (Left);
               L_Id : constant String8_Id := Get_String8_Id (Left);
               R_Id : constant String8_Id := Get_String8_Id (Right);
            begin
               if Get_String_Length (Right) /= Len then
                  return False;
               end if;
               for I in 1 .. Len loop
                  if Element_String8 (L_Id, I) /= Element_String8 (R_Id, I)
                  then
                     return False;
                  end if;
               end loop;
               return True;
            end;

         when Iir_Kind_Aggregate =>
            if not Are_Trees_Equal (Get_Type (Left), Get_Type (Right)) then
               return False;
            end if;
            return Are_Trees_Chain_Equal
              (Get_Association_Choices_Chain (Left),
               Get_Association_Choices_Chain (Right));

         when Iir_Kind_Choice_By_None
              | Iir_Kind_Choice_By_Others =>
            return Are_Trees_Equal (Get_Associated_Expr (Left),
                                    Get_Associated_Expr (Right));
         when Iir_Kind_Choice_By_Name =>
            if not Are_Trees_Equal (Get_Choice_Name (Left),
                                    Get_Choice_Name (Right))
            then
               return False;
            end if;
            return Are_Trees_Equal (Get_Associated_Expr (Left),
                                    Get_Associated_Expr (Right));
         when Iir_Kind_Choice_By_Expression =>
            if not Are_Trees_Equal (Get_Choice_Expression (Left),
                                    Get_Choice_Expression (Right)) then
               return False;
            end if;
            return Are_Trees_Equal (Get_Associated_Expr (Left),
                                    Get_Associated_Expr (Right));
         when Iir_Kind_Choice_By_Range =>
            if not Are_Trees_Equal (Get_Choice_Range (Left),
                                    Get_Choice_Range (Right)) then
               return False;
            end if;
            return Are_Trees_Equal (Get_Associated_Expr (Left),
                                    Get_Associated_Expr (Right));
         when Iir_Kind_Character_Literal =>
            return Are_Trees_Equal (Get_Named_Entity (Left),
                                    Get_Named_Entity (Right));
         when Iir_Kind_Allocator_By_Subtype =>
            return Are_Trees_Equal (Get_Subtype_Indication (Left),
                                    Get_Subtype_Indication (Right));
         when Iir_Kind_Allocator_By_Expression =>
            return Are_Trees_Equal (Get_Expression (Left),
                                    Get_Expression (Right));
         when others =>
            Error_Kind ("are_trees_equal", Left);
      end case;
   end Are_Trees_Equal;

   --  LRM 2.7  Conformance Rules.
   procedure Check_Conformance_Rules (Subprg, Spec: Iir) is
   begin
      if not Are_Trees_Equal (Subprg, Spec) then
         --  FIXME: should explain why it does not conform ?
         Error_Msg_Sem
           (+Subprg, "body of %n does not conform with specification at %l",
            (+Subprg, +Spec));
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
         --  Should be sure DECL1 and DECL belongs to the same declarative
         --  region, ie DECL1 was not made visible via a USE clause.
         --
         --  Also, only check for explicitly subprograms (and not
         --  implicit one).
         if not Is_Implicit_Subprogram (Decl1)
           and then Get_Kind (Decl1) in Iir_Kinds_Subprogram_Declaration
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
      Id : constant Name_Id := Get_Identifier (Decl);
      Inter : Name_Interpretation_Type;
      Prev : Iir;
      Num : Iir_Int32;
   begin
      Inter := Get_Interpretation (Id);
      while Valid_Interpretation (Inter)
        and then Is_In_Current_Declarative_Region (Inter)
      loop
         --  There is a previous declaration with the same name in the
         --  current declarative region.
         Prev := Get_Declaration (Inter);
         case Get_Kind (Prev) is
            when Iir_Kind_Function_Declaration
              | Iir_Kind_Procedure_Declaration =>
               if Is_Implicit_Subprogram (Prev) then
                  --  Implicit declarations aren't taken into account (as they
                  --  are mangled differently).
                  Inter := Get_Next_Interpretation (Inter);
               else
                  --  The previous declaration is a user subprogram.
                  Num := Get_Overload_Number (Prev) + 1;
                  if Num = 1
                    and then Get_Parent (Prev) = Get_Parent (Decl)
                  then
                     --  The previous was not (yet) overloaded.  Mark it as
                     --  overloaded.
                     --  Do not mark it if it is not in the same declarative
                     --  part (ie, do not change a subprogram declaration in
                     -- the package while analyzing the body).
                     Set_Overload_Number (Prev, 1);
                     Num := 2;
                  end if;
                  Set_Overload_Number (Decl, Num);
                  return;
               end if;
            when Iir_Kind_Enumeration_Literal =>
               --  Enumeration literal are ignored for overload number.
               Inter := Get_Next_Interpretation (Inter);
            when Iir_Kind_Non_Object_Alias_Declaration =>
               --  Subprogram aliases aren't considered, just skip them.
               --  (No subprogram is created by an alias).
               Inter := Get_Next_Interpretation (Inter);
            when others =>
               --  Case of user error: redefinition of an identifier.
               --  Error message is generated by sem_scope.
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
      Nbr_Interfaces := Vhdl.Nodes_Utils.Get_Chain_Length
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
            Error_Msg_Sem
              (+Subprg, "unary operator must have a single parameter");
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
              (+Subprg, "binary operators must have two parameters");
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
                 (+Subprg,
                  "logical operators must have two parameters before vhdl08");
            else
               Error_Msg_Sem
                 (+Subprg, "logical operators must have two parameters");
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
              (+Subprg,
               """+"" and ""-"" operators must have 1 or 2 parameters");
         when others =>
            return;
      end case;
      if Is_Method then
         Error_Msg_Sem
           (+Subprg,
            " (the protected object is an implicit parameter of methods)");
      end if;
   end Check_Operator_Requirements;

   procedure Sem_Subprogram_Specification (Subprg: Iir)
   is
      Interface_Chain : Iir;
      Return_Type : Iir;
   begin
      --  LRM 10.1 Declarative Region
      --  3. A subprogram declaration, together with the corresponding
      --     subprogram body.
      Open_Declarative_Region;

      -- Sem generics.
      if Get_Kind (Subprg) in Iir_Kinds_Subprogram_Declaration then
         Sem_Interface_Chain
           (Get_Generic_Chain (Subprg), Generic_Interface_List);
      end if;

      --  Sem interfaces.
      Interface_Chain := Get_Interface_Declaration_Chain (Subprg);
      case Get_Kind (Subprg) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Interface_Function_Declaration =>
            Sem_Interface_Chain
              (Interface_Chain, Function_Parameter_Interface_List);
            Return_Type := Get_Return_Type_Mark (Subprg);
            Return_Type := Sem_Type_Mark (Return_Type);
            Set_Return_Type_Mark (Subprg, Return_Type);
            Return_Type := Get_Type (Return_Type);
            Set_Return_Type (Subprg, Return_Type);
            Set_All_Sensitized_State (Subprg, Unknown);

            --  LRM08 4.2 Subprogram declarations
            --  It is an error if the result subtype of a function denotes
            --  either a file type or a protected type.  Moreover, it is an
            --  error if the result subtype of a pure function denotes an
            --  access type or a subtype that has a subelement of an access
            --  type.

            --  GHDL: this was added by VHDL 2008, but vital packages don't
            --  follow that rule.  So, it is not retroactive.
            case Get_Kind (Return_Type) is
               when Iir_Kind_File_Type_Definition =>
                  Error_Msg_Sem
                    (+Subprg, "result subtype cannot denote a file type");
               when Iir_Kind_Protected_Type_Declaration =>
                  Error_Msg_Sem
                    (+Subprg, "result subtype cannot denote a protected type");
               when Iir_Kind_Access_Type_Definition
                 | Iir_Kind_Access_Subtype_Definition =>
                  if Vhdl_Std >= Vhdl_08
                    and then Get_Pure_Flag (Subprg)
                  then
                     Error_Msg_Sem_Relaxed
                       (Subprg, Warnid_Pure,
                        "result subtype of a pure function cannot denote an"
                          & " access type");
                  end if;
               when others =>
                  if  Vhdl_Std >= Vhdl_08
                    and then not Get_Signal_Type_Flag (Return_Type)
                    and then Get_Pure_Flag (Subprg)
                  then
                     Error_Msg_Sem_Relaxed
                       (Subprg, Warnid_Pure,
                        "result subtype of a pure function cannot have"
                          & " access subelements");
                  end if;
            end case;

         when Iir_Kind_Interface_Procedure_Declaration =>
            Sem_Interface_Chain
              (Interface_Chain, Procedure_Parameter_Interface_List);

         when Iir_Kind_Procedure_Declaration =>
            Sem_Interface_Chain
              (Interface_Chain, Procedure_Parameter_Interface_List);

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
                  if Get_Kind (Inter) = Iir_Kind_Interface_Signal_Declaration
                    and then Get_Mode (Inter) /= Iir_In_Mode
                  then
                     --  There is a driver for this signal interface.
                     Set_Passive_Flag (Subprg, False);
                     exit;
                  end if;
                  Inter := Get_Chain (Inter);
               end loop;
            end;

            --  Mark the procedure as suspendable, unless in a std or
            --  most ieee packages.
            --  This is a minor optimization.
            declare
               Lib : constant Iir :=
                 Get_Library (Get_Design_File (Get_Current_Design_Unit));
            begin
               if Lib = Libraries.Std_Library then
                  --  No procedures in std have a wait statement.
                  null;
               elsif Get_Identifier (Lib) = Std_Names.Name_Ieee then
                  --  Package ieee.vital_primitives has wait statements.
                  declare
                     Unit : constant Iir :=
                       Get_Library_Unit (Get_Current_Design_Unit);
                     Unit_Id : constant Name_Id := Get_Identifier (Unit);
                  begin
                     if Unit_Id = Std_Names.Name_VITAL_Primitives then
                        Set_Suspend_Flag (Subprg, True);
                     end if;
                  end;
               else
                  --  User procedures may have wait statements.
                  Set_Suspend_Flag (Subprg, True);
               end if;
            end;
         when others =>
            Error_Kind ("sem_subprogram_declaration", Subprg);
      end case;

      Check_Operator_Requirements (Get_Identifier (Subprg), Subprg);

      Sem_Utils.Compute_Subprogram_Hash (Subprg);

      --  The specification has been analyzed, close the declarative region
      --  now.
      Close_Declarative_Region;
   end Sem_Subprogram_Specification;

   --  LRM 2.1  Subprogram Declarations.
   procedure Sem_Subprogram_Declaration (Subprg: Iir)
   is
      Parent : constant Iir := Get_Parent (Subprg);
      Spec: Iir;
      Subprg_Body : Iir;
   begin
      --  Set depth.
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
            --  FIXME: protected type ?
            Set_Subprogram_Depth (Subprg, 0);
      end case;

      Sem_Subprogram_Specification (Subprg);

      --  Look if there is an associated body (the next node).
      Subprg_Body := Get_Chain (Subprg);
      if Subprg_Body /= Null_Iir
        and then Get_Kind (Subprg_Body) in Iir_Kinds_Subprogram_Body
      then
         Spec := Find_Subprogram_Specification (Subprg);
      else
         Spec := Null_Iir;
      end if;

      if Spec /= Null_Iir then
         -- SUBPRG is the body of the specification SPEC.
         if Get_Subprogram_Body (Spec) /= Null_Iir then
            Error_Msg_Sem (+Subprg, "%n body already defined at %l",
                           (+Spec, +Get_Subprogram_Body (Spec)));
            --  Kill warning.
            Set_Use_Flag (Subprg, True);
         else
            Check_Conformance_Rules (Subprg, Spec);
            Xref_Body (Subprg, Spec);
            Set_Subprogram_Body (Subprg, Subprg_Body);
            Set_Subprogram_Specification (Subprg_Body, Spec);
            Set_Subprogram_Body (Spec, Subprg_Body);
         end if;
      else
         --  Forward declaration or specification followed by body.
         Set_Subprogram_Overload_Number (Subprg);
         Sem_Scopes.Add_Name (Subprg);
         Name_Visible (Subprg);
         Xref_Decl (Subprg);
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
      Spec : constant Iir := Get_Subprogram_Specification (Subprg);
      Warn_Hide_Enabled : constant Boolean := Is_Warning_Enabled (Warnid_Hide);
      El : Iir;
   begin
      Set_Impure_Depth (Subprg, Iir_Depth_Pure);

      --  LRM 10.1  Declarative regions
      --  3.  A subprogram declaration, together with the corresponding
      --     subprogram body.
      Open_Declarative_Region;
      Set_Is_Within_Flag (Spec, True);

      --  Add the interface names into the current declarative region.
      --  (Do not emit warnings for hiding, they were already emitted during
      --   analysis of the subprogram spec).
      Enable_Warning (Warnid_Hide, False);
      El := Get_Interface_Declaration_Chain (Spec);
      while El /= Null_Iir loop
         Add_Name (El, Get_Identifier (El), False);
         if Get_Kind (El) = Iir_Kind_Interface_Signal_Declaration then
            Set_Has_Active_Flag (El, False);
         end if;
         El := Get_Chain (El);
      end loop;
      Enable_Warning (Warnid_Hide, Warn_Hide_Enabled);

      --  For vhdl-19, handle return identifier.
      if Get_Kind (Spec) = Iir_Kind_Function_Declaration then
         declare
            Ret : constant Iir := Get_Return_Identifier (Spec);
            Ret_Type : Iir;
         begin
            if Ret /= Null_Iir then
               Xrefs.Xref_Decl (Ret);
               Set_Visible_Flag (Ret, True);
               Ret_Type := Get_Type (Spec);
               Ret_Type := Sem_Types.Build_Constrained_Subtype (Ret_Type, Ret);
               Set_Type (Ret, Ret_Type);
               Add_Name (Ret, Get_Identifier (Ret), False);
            end if;
         end;
      end if;

      Sem_Sequential_Statements (Spec, Subprg);

      Set_Is_Within_Flag (Spec, False);
      Close_Declarative_Region;

      case Get_Kind (Spec) is
         when Iir_Kind_Procedure_Declaration =>
            if Get_Suspend_Flag (Subprg)
              and then not Get_Suspend_Flag (Spec)
            then
               --  Incoherence: procedures declared in std library are not
               --  expected to suspend.  This is an internal check.
               Error_Msg_Sem (+Subprg, "unexpected suspendable procedure");
            end if;

            --  Update purity state of procedure if there are no callees.
            case Get_Purity_State (Spec) is
               when Pure
                 | Maybe_Impure =>
                  --  We can't know this yet.
                  raise Internal_Error;
               when Impure =>
                  null;
               when Unknown =>
                  if Get_Callees_List (Subprg) = Null_Iir_List then
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
                  Callees : constant Iir_List := Get_Callees_List (Subprg);
                  Callees_It : List_Iterator;
                  Callee : Iir;
                  State : Tri_State_Type;
               begin
                  --  Per default, has no wait.
                  Set_Wait_State (Spec, False);
                  Callees_It := List_Iterate_Safe (Callees);
                  while Is_Valid (Callees_It) loop
                     Callee := Get_Element (Callees_It);
                     case Get_Kind (Callee) is
                        when Iir_Kind_Function_Declaration =>
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
                        when others =>
                           Error_Kind ("sem_subprogram_body(2)", Callee);
                     end case;
                     Next (Callees_It);
                  end loop;
               end;
            end if;

            --  Do not add to Analysis_Checks_List as procedures can't
            --  generate purity/wait/all-sensitized errors by themselves.

         when Iir_Kind_Function_Declaration =>
            if Get_Callees_List (Subprg) /= Null_Iir_List then
               --  Purity calls to be checked later.
               --  No wait statements in procedures called.
               Add_Analysis_Checks_List (Spec);
            end if;
         when others =>
            Error_Kind ("sem_subprogram_body", Spec);
      end case;

      --  Set All_Sensitized_State in trivial cases.
      if Get_All_Sensitized_State (Spec) = Unknown
        and then Get_Callees_List (Subprg) = Null_Iir_List
      then
         Set_All_Sensitized_State (Spec, No_Signal);
      end if;
   end Sem_Subprogram_Body;

   function Sem_Uninstantiated_Subprogram_Name (Decl : Iir) return Iir
   is
      Name : Iir;
      Subprg : Iir;
   begin
      Name := Get_Uninstantiated_Subprogram_Name (Decl);
      if Get_Kind (Name) = Iir_Kind_Signature then
         --  TODO
         raise Internal_Error;
      end if;

      Name := Sem_Denoting_Name (Name);
      Set_Uninstantiated_Subprogram_Name (Decl, Name);
      Subprg := Get_Named_Entity (Name);
      if Is_Error (Subprg) then
         return Subprg;
      end if;

      if Is_Overload_List (Subprg) then
         --  TODO
         raise Internal_Error;
      end if;

      if Get_Kind (Subprg) not in Iir_Kinds_Subprogram_Declaration then
         Error_Class_Match (Name, "package");
         return Create_Error (Subprg);
      end if;

      case Get_Kind (Decl) is
         when Iir_Kind_Procedure_Instantiation_Declaration =>
            if Get_Kind (Subprg) /= Iir_Kind_Procedure_Declaration then
               Error_Msg_Sem
                 (+Name,
                  "a procedure instantiation cannot instantiate %i", +Subprg);
               return Create_Error (Subprg);
            end if;
         when Iir_Kind_Function_Instantiation_Declaration =>
            if Get_Kind (Subprg) /= Iir_Kind_Function_Declaration then
               Error_Msg_Sem
                 (+Name,
                  "a function instantiation cannot instantiate %i", +Subprg);
               return Create_Error (Subprg);
            end if;
         when others =>
            raise Internal_Error;
      end case;

      if not Is_Uninstantiated_Subprogram (Subprg) then
         Error_Msg_Sem
           (+Name, "%n is not an uninstantiated subprogram", +Subprg);
         return Create_Error (Subprg);
      end if;

      return Subprg;
   end Sem_Uninstantiated_Subprogram_Name;

   procedure Sem_Subprogram_Instantiation_Declaration (Decl : Iir)
   is
      Subprg : Iir;
   begin
      Xref_Decl (Decl);

      Subprg := Sem_Uninstantiated_Subprogram_Name (Decl);
      if Subprg = Null_Iir or Is_Error (Subprg) then
         --  What could be done ?
         return;
      end if;

      --  LRM08 4.4 Subprogram instantiation declarations
      --  The generic map aspect, if present, optionally associates a single
      --  actual with each formal generic (or member thereof) in the
      --  corresponding subprogram declaration.  Each formal generic (of member
      --  thereof) shall be associated at most once.
      if not Sem_Generic_Association_Chain (Subprg, Decl) then
         --  FIXME: stop analysis here ?
         return;
      end if;

      --  Create the interface parameters.
      Sem_Inst.Instantiate_Subprogram_Declaration (Decl, Subprg);

      --  Add DECL.  Must be done after parameters creation to handle
      --  homographs.
      Sem_Scopes.Add_Name (Decl);
      Set_Visible_Flag (Decl, True);
   end Sem_Subprogram_Instantiation_Declaration;

   --  Return the subprogram body of SPEC.  If there is no body, and if SPEC
   --  is an instance, returns the body of the generic specification but only
   --  if known.
   function Get_Subprogram_Body_Or_Generic (Spec : Iir) return Iir
   is
      Bod : Iir;
      Orig : Iir;
   begin
      Bod := Get_Subprogram_Body (Spec);

      if Bod /= Null_Iir then
         return Bod;
      end if;

      Orig := Sem_Inst.Get_Origin (Spec);
      if Orig = Null_Iir then
         return Null_Iir;
      end if;

      return Get_Subprogram_Body (Orig);
   end Get_Subprogram_Body_Or_Generic;

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

   function Update_And_Check_Pure_Wait (Subprg : Iir) return Update_Pure_Status
   is
      procedure Error_Wait (Caller : Iir; Callee : Iir) is
      begin
         Report_Start_Group;
         Error_Msg_Sem
           (+Caller, "%n must not contain wait statement, but calls", +Caller);
         Error_Msg_Sem
           (+Callee, "%n which has (indirectly) a wait statement", +Callee);
         Report_End_Group;
      end Error_Wait;

      --  Kind of subprg.
      type Caller_Kind is (K_Function, K_Process, K_Procedure);
      Kind : Caller_Kind;

      Callees_List : Iir_List;
      Callees_List_Holder : Iir;
      Callees_It : List_Iterator;
      Callee : Iir;
      Callee_Bod : Iir;
      Subprg_Depth : Iir_Int32;
      Subprg_Bod : Iir;
      --  Current purity depth of SUBPRG.
      Depth : Iir_Int32;
      Depth_Callee : Iir_Int32;
      Has_Wait_Errors : Boolean := False;
      New_List : Iir_List;
      Res, Res1 : Update_Pure_Status;
   begin
      case Get_Kind (Subprg) is
         when Iir_Kind_Function_Declaration =>
            Kind := K_Function;
            Subprg_Bod := Get_Subprogram_Body_Or_Generic (Subprg);
            if Subprg_Bod = Null_Iir then
               return Update_Pure_Missing;
            end if;
            Subprg_Depth := Get_Subprogram_Depth (Subprg);
            Callees_List_Holder := Subprg_Bod;
            if Get_Pure_Flag (Subprg) then
               Depth := Iir_Depth_Pure;
            else
               Depth := Iir_Depth_Impure;
            end if;

         when Iir_Kind_Procedure_Declaration =>
            Kind := K_Procedure;
            Subprg_Bod := Get_Subprogram_Body_Or_Generic (Subprg);
            if Subprg_Bod = Null_Iir then
               return Update_Pure_Missing;
            end if;
            if Get_Purity_State (Subprg) = Impure
              and then Get_Wait_State (Subprg) /= Unknown
              and then Get_All_Sensitized_State (Subprg) /= Unknown
            then
               --  No need to go further.
               if Get_All_Sensitized_State (Subprg) = No_Signal
                 or else Vhdl_Std < Vhdl_08
               then
                  Callees_List := Get_Callees_List (Subprg_Bod);
                  Destroy_Iir_List (Callees_List);
                  Set_Callees_List (Subprg_Bod, Null_Iir_List);
               end if;
               return Update_Pure_Done;
            end if;
            Subprg_Depth := Get_Subprogram_Depth (Subprg);
            Depth := Get_Impure_Depth (Subprg_Bod);
            Callees_List_Holder := Subprg_Bod;

         when Iir_Kind_Sensitized_Process_Statement =>
            Kind := K_Process;
            Subprg_Bod := Null_Iir;
            Subprg_Depth := Iir_Depth_Top;
            Depth := Iir_Depth_Impure;
            Callees_List_Holder := Subprg;

         when others =>
            Error_Kind ("update_and_check_pure_wait(1)", Subprg);
      end case;

      --  If the subprogram has no callee list, there is nothing to do.
      Callees_List := Get_Callees_List (Callees_List_Holder);
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
      Set_Callees_List (Callees_List_Holder, Null_Iir_List);

      --  First loop: check without recursion.
      --  Second loop: recurse if necessary.
      for J in 0 .. 1 loop
         New_List := Create_Iir_List;
         Callees_It := List_Iterate (Callees_List);
         while Is_Valid (Callees_It) loop
            Callee := Get_Element (Callees_It);

            --  Note:
            --  Pure functions should not be in the list.
            --  Impure functions must have directly set Purity_State.

            --  The body of subprograms may not be set for instances.
            --  Use the body from the generic (if any).
            --  This is meaningful for non macro-expanded package interface,
            --  because there is no associated body and because the call
            --  tree is known (if there were an interface subprogram, it
            --  would have been macro-expanded).
            --  Do not set the body, as it would trigger an assert during
            --  macro-expansion (maybe this shouldn't be called for macro
            --  expanded packages).
            Callee_Bod := Get_Subprogram_Body_Or_Generic (Callee);

            --  Check pure.
            if Callee_Bod = Null_Iir then
               --  No body yet for the subprogram called.
               --  Nothing can be extracted from it, postpone the checks until
               --  elaboration.
               Res := Update_Pure_Missing;
            else
               --  Second loop: recurse if a state is not known.
               if J = 1
                 and then
                 ((Get_Kind (Callee) = Iir_Kind_Procedure_Declaration
                     and then Get_Purity_State (Callee) = Unknown)
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
                        --  FIXME: report call location
                        Error_Pure (Elaboration, Subprg_Bod, Callee, Null_Iir);
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
                     Report_Start_Group;
                     Error_Msg_Sem
                       (+Subprg, "all-sensitized %n can't call %n",
                        (+Subprg, +Callee));
                     Error_Msg_Sem
                       (+Subprg,
                        " (as this subprogram reads (indirectly) a signal)");
                     Report_End_Group;
               end case;
            end if;

            --  Keep in list.
            if Callee_Bod = Null_Iir
              or else
              (Get_Kind (Callee) = Iir_Kind_Procedure_Declaration
                 and then Get_Purity_State (Callee) = Unknown
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
               Append_Element (New_List, Callee);
            end if;
            Next (Callees_It);
         end loop;

         --  End of callee loop.
         if Is_Empty (New_List) then
            Destroy_Iir_List (Callees_List);
            Callees_List := Null_Iir_List;
            Destroy_Iir_List (New_List);
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
            Destroy_Iir_List (Callees_List);
            Callees_List := New_List;
         end if;
      end loop;

      Set_Callees_List (Callees_List_Holder, New_List);

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
      List : Iir_List;
      El : Iir;
      It : List_Iterator;
      Keep : Boolean;
      New_List : Iir_List;
   begin
      List := Get_Analysis_Checks_List (Unit);
      if List = Null_Iir_List then
         --  Return now if there is nothing to check.
         return;
      end if;

      New_List := Create_Iir_List;
      It := List_Iterate (List);
      while Is_Valid (It) loop
         El := Get_Element (It);
         Keep := False;
         case Get_Kind (El) is
            when Iir_Kind_Function_Declaration =>
               --  FIXME: remove from list if fully tested ?
               if not Root_Update_And_Check_Pure_Wait (El) then
                  Keep := True;
                  if Emit_Warnings then
                     declare
                        Bod : constant Iir := Get_Subprogram_Body (El);
                        Callees : constant Iir_List := Get_Callees_List (Bod);
                        pragma Assert (Callees /= Null_Iir_List);
                        Callee : constant Iir := Get_First_Element (Callees);
                     begin
                        Report_Start_Group;
                        Warning_Msg_Sem
                          (Warnid_Delayed_Checks, +El,
                           "can't assert that all calls in %n"
                             & " are pure or have not wait;"
                             & " will be checked at elaboration", +El);
                        --  FIXME: could improve this message by displaying
                        --  the chain of calls until the first subprograms in
                        --  unknown state.
                        Warning_Msg_Sem
                          (Warnid_Delayed_Checks, +Callee,
                           "(first such call is to %n)", +Callee);
                        Report_End_Group;
                     end;
                  end if;
               end if;
            when Iir_Kind_Sensitized_Process_Statement =>
               if not Root_Update_And_Check_Pure_Wait (El) then
                  Keep := True;
                  if Emit_Warnings then
                     Warning_Msg_Sem
                       (Warnid_Delayed_Checks, +El,
                        "can't assert that %n has no wait; "
                          & "will be checked at elaboration", +El);
                  end if;
               end if;
            when others =>
               Error_Kind ("sem_analysis_checks_list", El);
         end case;
         if Keep then
            Append_Element (New_List, El);
         end if;
         Next (It);
      end loop;
      if Is_Empty (New_List) then
         Destroy_Iir_List (New_List);
         New_List := Null_Iir_List;  --  OK, redundant but clearer.
      end if;
      Destroy_Iir_List (List);
      Set_Analysis_Checks_List (Unit, New_List);
   end Sem_Analysis_Checks_List;

   --  Return true if package declaration DECL needs a body.
   --  Ie, it contains subprogram specification or deferred constants.
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
               if not Is_Implicit_Subprogram (El) then
                  return True;
               end if;
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
               Def := Get_Type_Definition (El);
               if Def /= Null_Iir
                 and then Get_Kind (Def) = Iir_Kind_Protected_Type_Declaration
               then
                  return True;
               end if;
            when Iir_Kind_Anonymous_Type_Declaration
              | Iir_Kind_Subtype_Declaration =>
               null;
            when Iir_Kind_Attribute_Declaration
              | Iir_Kind_Attribute_Specification =>
               null;
            when Iir_Kind_Disconnection_Specification =>
               null;
            when Iir_Kind_Use_Clause =>
               null;
            when Iir_Kind_Component_Declaration =>
               null;
            when Iir_Kind_Protected_Type_Body =>
               null;
            when Iir_Kind_Package_Declaration =>
               --  LRM08 4.8 Package bodies
               --  A package body that is not a library unit shall appear
               --  immediately within the same declarative region as the
               --  corresponding package declaration and textually subsequent
               --  to that package declaration.
               if Get_Need_Body (El) then
                  return True;
               end if;
            when Iir_Kind_Package_Body =>
               null;
            when Iir_Kind_Package_Instantiation_Declaration =>
               null;
            when Iir_Kind_Nature_Declaration
              | Iir_Kind_Subnature_Declaration =>
               null;
            when Iir_Kind_Terminal_Declaration =>
               null;
            when others =>
               pragma Assert (Flags.Flag_Force_Analysis);
               null;
         end case;
         El := Get_Chain (El);
      end loop;
      return False;
   end Package_Need_Body_P;

   --  Return true if package declaration DECL contains at least one package
   --  instantiation that needs a body.
   function Package_Need_Instance_Bodies_P (Decl: Iir_Package_Declaration)
                                           return Boolean
   is
      El: Iir;
   begin
      El := Get_Declaration_Chain (Decl);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Package_Instantiation_Declaration =>
               declare
                  Pkg : constant Iir := Get_Uninstantiated_Package_Decl (El);
               begin
                  if not Is_Error (Pkg)
                    and then Get_Need_Body (Pkg)
                  then
                     return True;
                  end if;
               end;
            when others =>
               null;
         end case;
         El := Get_Chain (El);
      end loop;
      return False;
   end Package_Need_Instance_Bodies_P;

   --  Return true if uninstantiated pckage DECL must be macro-expanded (at
   --  least one interface type).
   function Is_Package_Macro_Expanded
     (Decl : Iir_Package_Declaration) return Boolean
   is
      Header : constant Iir := Get_Package_Header (Decl);
      Inter : Iir;
   begin
      Inter := Get_Generic_Chain (Header);
      while Is_Valid (Inter) loop
         case Iir_Kinds_Interface_Declaration (Get_Kind (Inter)) is
            when Iir_Kinds_Interface_Object_Declaration
              | Iir_Kind_Interface_Terminal_Declaration =>
               null;
            when Iir_Kind_Interface_Type_Declaration =>
               return True;
            when Iir_Kind_Interface_Package_Declaration =>
               declare
                  Pkg : constant Iir :=
                    Get_Uninstantiated_Package_Decl (Inter);
               begin
                  --  Could be an error.
                  if Get_Kind (Pkg) = Iir_Kind_Package_Declaration
                    and then Get_Macro_Expanded_Flag (Pkg)
                  then
                     return True;
                  end if;
               end;
            when Iir_Kinds_Interface_Subprogram_Declaration =>
               return True;
         end case;
         Inter := Get_Chain (Inter);
      end loop;
      return False;
   end Is_Package_Macro_Expanded;

   --  LRM 2.5  Package Declarations.
   procedure Sem_Package_Declaration (Pkg : Iir_Package_Declaration)
   is
      Unit : constant Iir_Design_Unit := Get_Design_Unit (Pkg);
      Header : constant Iir := Get_Package_Header (Pkg);
      Implicit : Implicit_Signal_Declaration_Type;
   begin
      Sem_Scopes.Add_Name (Pkg);
      Set_Visible_Flag (Pkg, True);
      Xref_Decl (Pkg);

      Set_Is_Within_Flag (Pkg, True);

      --  Identify IEEE.Std_Logic_1164 for VHDL08.
      if Get_Identifier (Pkg) = Std_Names.Name_Std_Logic_1164
        and then (Get_Identifier (Get_Library (Get_Design_File (Unit)))
                    = Std_Names.Name_Ieee)
      then
         Vhdl.Ieee.Std_Logic_1164.Std_Logic_1164_Pkg := Pkg;
      end if;

      --  LRM93 10.1 Declarative Region
      --  4. A package declaration, together with the corresponding
      --     body (if any).
      Open_Declarative_Region;

      Push_Signals_Declarative_Part (Implicit, Pkg);

      if Header /= Null_Iir then
         declare
            Generic_Chain : constant Iir := Get_Generic_Chain (Header);
            Generic_Map : constant Iir :=
              Get_Generic_Map_Aspect_Chain (Header);
            Assoc_El : Iir;
            Inter_El : Iir;
            Inter : Iir;
         begin
            Sem_Interface_Chain (Generic_Chain, Generic_Interface_List);

            if Generic_Map /= Null_Iir then
               --  Generic-mapped packages are not macro-expanded.
               Set_Macro_Expanded_Flag (Pkg, False);

               if Sem_Generic_Association_Chain (Header, Header) then
                  --  For generic-mapped packages, use the actual type for
                  --  interface type.
                  Assoc_El := Get_Generic_Map_Aspect_Chain (Header);
                  Inter_El := Generic_Chain;
                  while Is_Valid (Assoc_El) loop
                     if Get_Kind (Assoc_El) = Iir_Kind_Association_Element_Type
                     then
                        Inter :=
                          Get_Association_Interface (Assoc_El, Inter_El);
                        Sem_Inst.Substitute_On_Chain
                          (Generic_Chain,
                           Get_Type (Inter),
                           Get_Type (Get_Named_Entity
                                       (Get_Actual (Assoc_El))));
                     end if;
                     Next_Association_Interface (Assoc_El, Inter_El);
                  end loop;
               end if;
            else
               --  Uninstantiated package.  Maybe macro expanded.
               Set_Macro_Expanded_Flag
                 (Pkg, Is_Package_Macro_Expanded (Pkg));
            end if;
         end;
      else
         --  Simple packages are never expanded.
         Set_Macro_Expanded_Flag (Pkg, False);
      end if;

      Sem_Declaration_Chain (Pkg);
      --  GHDL: subprogram bodies appear in package body.

      Pop_Signals_Declarative_Part (Implicit);
      Close_Declarative_Region;
      Set_Is_Within_Flag (Pkg, False);

      Set_Need_Body (Pkg, Package_Need_Body_P (Pkg));

      if Vhdl_Std >= Vhdl_08 then
         Set_Need_Instance_Bodies
           (Pkg, Package_Need_Instance_Bodies_P (Pkg));
      end if;
   end Sem_Package_Declaration;

   --  LRM 2.6  Package Bodies.
   procedure Sem_Package_Body (Decl : Iir)
   is
      Package_Ident : constant Name_Id := Get_Identifier (Decl);
      Package_Decl : Iir;
   begin
      -- First, find the package declaration.
      if not Is_Nested_Package (Decl) then
         declare
            Design_Unit: Iir_Design_Unit;
         begin
            Design_Unit := Load_Primary_Unit
              (Get_Library (Get_Design_File (Get_Current_Design_Unit)),
               Package_Ident, Decl);
            if Design_Unit = Null_Iir then
               Error_Msg_Sem
                 (+Decl, "package %i was not analysed", +Package_Ident);
               return;
            end if;

            Package_Decl := Get_Library_Unit (Design_Unit);
            if Get_Kind (Package_Decl) /= Iir_Kind_Package_Declaration then
               Error_Msg_Sem
                 (+Decl, "primary unit %i is not a package", +Package_Ident);
               return;
            end if;

            --  LRM08 13.5 Order of analysis
            --  In each case, the second unit depends on the first unit
            Add_Dependence (Design_Unit);

            Add_Name (Design_Unit);

            --  Add the context clauses from the primary unit.
            Add_Context_Clauses (Design_Unit);
         end;
      else
         declare
            Interp : Name_Interpretation_Type;
         begin
            Interp := Get_Interpretation (Get_Identifier (Decl));
            if not Valid_Interpretation (Interp)
              or else not Is_In_Current_Declarative_Region (Interp)
              or else Is_Potentially_Visible (Interp)
            then
               Error_Msg_Sem
                 (+Decl, "no corresponding package declaration for %i",
                  +Package_Ident);
               return;
            end if;

            Package_Decl := Get_Declaration (Interp);
            if Get_Kind (Package_Decl) /= Iir_Kind_Package_Declaration then
               Error_Msg_Sem
                 (+Decl, "declaration %i is not a package", +Package_Ident);
               return;
            end if;
         end;
      end if;

      --  Emit a warning is a body is not necessary.
      if not Get_Need_Body (Package_Decl) then
         Warning_Msg_Sem (Warnid_Body, +Decl,
                          "%n does not require a body", +Package_Decl);
      end if;

      Set_Package (Decl, Package_Decl);
      Xref_Body (Decl, Package_Decl);
      Set_Package_Body (Package_Decl, Decl);
      Set_Is_Within_Flag (Package_Decl, True);

      --  LRM93 10.1 Declarative Region
      --  4. A package declaration, together with the corresponding
      --     body (if any).
      Open_Declarative_Region;

      Sem_Scopes.Add_Package_Declarations (Package_Decl);

      Sem_Declaration_Chain (Decl);
      Check_Full_Declaration (Decl, Decl);
      Check_Full_Declaration (Package_Decl, Decl);

      Close_Declarative_Region;
      Set_Is_Within_Flag (Package_Decl, False);
   end Sem_Package_Body;

   function Sem_Uninstantiated_Package_Name (Decl : Iir) return Iir
   is
      Name : Iir;
      Pkg : Iir;
   begin
      Name := Sem_Denoting_Name (Get_Uninstantiated_Package_Name (Decl));
      Set_Uninstantiated_Package_Name (Decl, Name);
      Pkg := Get_Named_Entity (Name);
      if Is_Error (Pkg) then
         null;
      elsif Get_Kind (Pkg) /= Iir_Kind_Package_Declaration then
         Error_Class_Match (Name, "package");
         Pkg := Create_Error (Pkg);
      elsif not Is_Uninstantiated_Package (Pkg) then
         Error_Msg_Sem (+Name, "%n is not an uninstantiated package", +Pkg);
         Pkg := Create_Error (Pkg);
      end if;

      Set_Uninstantiated_Package_Decl (Decl, Pkg);

      return Pkg;
   end Sem_Uninstantiated_Package_Name;

   --  LRM08 4.9  Package Instantiation Declaration
   procedure Sem_Package_Instantiation_Declaration (Decl : Iir)
   is
      Hdr : Iir;
      Pkg : Iir;
      Bod : Iir_Design_Unit;
   begin
      Sem_Scopes.Add_Name (Decl);
      Set_Visible_Flag (Decl, True);
      Xref_Decl (Decl);

      --  LRM08 4.9
      --  The uninstantiated package name shall denote an uninstantiated
      --  package declared in a package declaration.
      Pkg := Sem_Uninstantiated_Package_Name (Decl);
      if Pkg = Null_Iir or Is_Error (Pkg) then
         --  What could be done ?
         return;
      end if;

      --  LRM08 4.9
      --  The generic map aspect, if present, optionally associates a single
      --  actual with each formal generic (or member thereof) in the
      --  corresponding package declaration.  Each formal generic (or member
      --  thereof) shall be associated at most once.

      --  GHDL: the generics are first instantiated (ie copied) and then
      --  the actuals are associated with the instantiated formal.
      --  FIXME: do it in Instantiate_Package_Declaration ?
      Hdr := Get_Package_Header (Pkg);
      if not Sem_Generic_Association_Chain (Hdr, Decl) then
         --  FIXME: stop analysis here ?
         return;
      end if;

      --  FIXME: unless the parent is a package declaration library unit, the
      --  design unit depends on the body.
      if Get_Need_Body (Pkg) and then not Is_Nested_Package (Pkg) then
         Bod := Get_Package_Body (Pkg);
         if Is_Null (Bod) then
            Bod := Load_Secondary_Unit
              (Get_Design_Unit (Pkg), Null_Identifier, Decl);
         else
            Bod := Get_Design_Unit (Bod);
         end if;
         if Is_Null (Bod) then
            Error_Msg_Sem (+Decl, "cannot find package body of %n", +Pkg);
         else
            Add_Dependence (Bod);
         end if;
      end if;

      --  Instantiate the declaration after analyse of the body.  So that
      --  the use_flag on the declaration can be propagated to the instance.
      Sem_Inst.Instantiate_Package_Declaration (Decl, Pkg);
   end Sem_Package_Instantiation_Declaration;

   --  LRM 10.4  Use Clauses.
   procedure Sem_Use_Clause_Name (Clause : Iir)
   is
      Name: Iir;
      Prefix: Iir;
      Name_Prefix : Iir;
   begin
      --  LRM93 10.4
      --  A use clause achieves direct visibility of declarations that are
      --  visible by selection.
      --  Each selected name is a use clause identifies one or more
      --  declarations that will potentialy become directly visible.

      Name := Get_Selected_Name (Clause);
      if Name = Null_Iir then
         pragma Assert (Flags.Flag_Force_Analysis);
         return;
      end if;

      case Get_Kind (Name) is
         when Iir_Kind_Selected_By_All_Name
           | Iir_Kind_Selected_Name =>
            Name_Prefix := Get_Prefix (Name);
         when others =>
            Error_Msg_Sem (+Name, "use clause allows only selected name");
            Set_Selected_Name (Clause, Create_Error_Name (Name));
            return;
      end case;

      case Get_Kind (Name_Prefix) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name =>
            null;
         when others =>
            Error_Msg_Sem
              (+Name_Prefix,
               "use clause prefix must be a name or a selected name");
            Set_Selected_Name (Clause, Create_Error_Name (Name));
            return;
      end case;

      Name_Prefix := Sem_Denoting_Name (Name_Prefix);
      Set_Prefix (Name, Name_Prefix);
      Prefix := Get_Named_Entity (Name_Prefix);
      if Is_Error (Prefix) then
         Set_Selected_Name (Clause, Create_Error_Name (Name));
         return;
      end if;

      --  LRM 10.4 Use Clauses
      --
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
      case Get_Kind (Prefix) is
         when Iir_Kind_Library_Declaration =>
            null;
         when Iir_Kind_Package_Instantiation_Declaration
           | Iir_Kind_Interface_Package_Declaration =>
            null;
         when Iir_Kind_Package_Declaration =>
            --  LRM08 12.4 Use clauses
            --  It is an error if the prefix of a selected name in a use
            --  clause denotes an uninstantiated package.
            if Is_Uninstantiated_Package (Prefix) then
               Error_Msg_Sem
                 (+Name_Prefix,
                  "use of uninstantiated package is not allowed");
               Set_Prefix (Name, Create_Error_Name (Name_Prefix));
               return;
            end if;
         when others =>
            Error_Msg_Sem
              (+Name, "prefix must designate a package or a library");
            Set_Prefix (Name, Create_Error_Name (Name_Prefix));
            return;
      end case;

      case Get_Kind (Name) is
         when Iir_Kind_Selected_Name =>
            Sem_Name (Name, True);
            case Get_Kind (Get_Named_Entity (Name)) is
               when Iir_Kind_Error =>
                  --  Continue in case of error.
                  null;
               when Iir_Kind_Overload_List =>
                  --  Analyze is correct as is.
                  null;
               when others =>
                  Name := Finish_Sem_Name (Name);
                  Set_Selected_Name (Clause, Name);
            end case;
         when Iir_Kind_Selected_By_All_Name =>
            null;
         when others =>
            raise Internal_Error;
      end case;
   end Sem_Use_Clause_Name;

   --  LRM 10.4  Use Clauses.
   procedure Sem_Use_Clause (Clauses: Iir_Use_Clause)
   is
      Clause : Iir_Use_Clause;
   begin
      Clause := Clauses;
      loop
         Sem_Use_Clause_Name (Clause);

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
         Error_Msg_Sem (+Decl, "no resource library %i", +Ident);
      else
         Set_Library_Declaration (Decl, Lib);
         Sem_Scopes.Add_Name (Lib, Ident, False);
         Set_Visible_Flag (Lib, True);

         --  Override the location of the library.  Even if the library is
         --  not really declared by the library clause (it is almost pre-
         --  declared), it improves error messages.
         --  Note: library clauses of dependences will overwrite the location,
         --   defeating the purpose of it.
         Location_Copy (Lib, Decl);

         Xref_Ref (Decl, Lib);
      end if;
   end Sem_Library_Clause;

   --  LRM08 13.4 Context clauses.
   procedure Sem_One_Context_Reference (Ref : Iir)
   is
      Name : Iir;
      Ent : Iir;
   begin
      Name := Get_Selected_Name (Ref);
      if Get_Kind (Name) /= Iir_Kind_Selected_Name then
         Error_Msg_Sem
           (+Name, "context reference only allows selected names");
         return;
      end if;

      Name := Sem_Denoting_Name (Name);
      Set_Selected_Name (Ref, Name);
      Ent := Get_Named_Entity (Name);
      if Is_Error (Ent) then
         return;
      end if;

      --  LRM08 13.4 Context clauses
      --  It is an error if a selected name in a context reference does not
      --  denote a context declaration.
      if Get_Kind (Ent) /= Iir_Kind_Context_Declaration then
         Error_Msg_Sem (+Name, "name must denote a context declaration");
         Set_Named_Entity (Name, Null_Iir);
         return;
      end if;
   end Sem_One_Context_Reference;

   --  LRM08 13.4 Context clauses.
   procedure Sem_Context_Reference (Ctxt : Iir)
   is
      Ref : Iir;
   begin
      Ref := Ctxt;
      loop
         Sem_One_Context_Reference (Ref);
         Ref := Get_Context_Reference_Chain (Ref);
         exit when Ref = Null_Iir;
      end loop;

      --  FIXME: must be done clause after clause ?
      Add_Context_Reference (Ctxt);
   end Sem_Context_Reference;

   --  LRM 11.3  Context Clauses.
   procedure Sem_Context_Clauses (Unit: Iir)
   is
      El: Iir;
   begin
      El := Get_Context_Items (Unit);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Use_Clause =>
               Sem_Use_Clause (El);
            when Iir_Kind_Library_Clause =>
               Sem_Library_Clause (El);
            when Iir_Kind_Context_Reference =>
               Sem_Context_Reference (El);
            when others =>
               Error_Kind ("sem_context_clauses", El);
         end case;
         El := Get_Chain (El);
      end loop;
   end Sem_Context_Clauses;

   --  LRM08 13.3 Context declarations
   procedure Sem_Context_Declaration (Decl: Iir)
   is
      --  Return TRUE iff the first prefix of NAME denotes library WORK.
      function Has_Work_Library_Prefix (Name : Iir) return Boolean
      is
         Prefix : Iir;
      begin
         Prefix := Name;
         while Get_Kind (Prefix) = Iir_Kind_Selected_Name
           or else Get_Kind (Prefix) = Iir_Kind_Selected_By_All_Name
         loop
            Prefix := Get_Prefix (Prefix);
         end loop;
         return Get_Kind (Prefix) = Iir_Kind_Simple_Name
           and then Get_Identifier (Prefix) = Std_Names.Name_Work
           and then (Get_Kind (Get_Named_Entity (Prefix))
                       = Iir_Kind_Library_Declaration);
      end Has_Work_Library_Prefix;

      procedure Error_Work_Prefix (Loc : Iir) is
      begin
         Error_Msg_Sem
           (+Loc, "'work' not allowed as prefix in context declaration");
      end Error_Work_Prefix;

      El : Iir;
      El1 : Iir;
   begin
      Sem_Scopes.Add_Name (Decl);
      Xref_Decl (Decl);

      Sem_Context_Clauses (Decl);

      El := Get_Context_Items (Decl);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Library_Clause =>
               --  LRM08 13.3 Context declarations
               --  It is an error if a library clause in a context declaration
               --  defines the library logical name WORK, [...]
               if Get_Identifier (El) = Std_Names.Name_Work then
                  Error_Msg_Sem
                    (+El, "'library work' not allowed in context declaration");
               end if;
            when Iir_Kind_Use_Clause =>
               --  LRM08 13.3 Context declarations
               --  [...] or if a selected name in a use clause [or a context
               --  reference] in a context declaration has the library logic
               --  name WORK as a prefix.
               El1 := El;
               while El1 /= Null_Iir loop
                  if Has_Work_Library_Prefix (Get_Selected_Name (El1)) then
                     Error_Work_Prefix (El1);
                     exit;
                  end if;
                  El1 := Get_Use_Clause_Chain (El1);
               end loop;
            when Iir_Kind_Context_Reference =>
               --  LRM08 13.3 Context declarations
               --  [...] or if a selected name in [a use clause or] a context
               --  reference in a context declaration has the library logic
               --  name WORK as a prefix.
               El1 := El;
               while El1 /= Null_Iir loop
                  if Has_Work_Library_Prefix (Get_Selected_Name (El1)) then
                     Error_Work_Prefix (El1);
                     exit;
                  end if;
                  El1 := Get_Context_Reference_Chain (El1);
               end loop;
            when others =>
               raise Internal_Error;
         end case;
         El := Get_Chain (El);
      end loop;

      --  GHDL: forbid self-reference by making declaration visible at the end.
      --  This violates LRM08 12.3 Visibility:  A declaration is visible only
      --  within a certain part of its scope; ...
      Set_Visible_Flag (Decl, True);
   end Sem_Context_Declaration;

   -- Access to the current design unit.  This is set, saved, restored, cleared
   -- by the procedure semantic.
   Current_Design_Unit: Iir_Design_Unit := Null_Iir;

   function Get_Current_Design_Unit return Iir_Design_Unit is
   begin
      return Current_Design_Unit;
   end Get_Current_Design_Unit;

   --  LRM 11.1  Design units.
   procedure Semantic (Design_Unit : Iir_Design_Unit)
   is
      Library_Unit : constant Iir := Get_Library_Unit (Design_Unit);
      Library : constant Iir := Get_Library (Get_Design_File (Design_Unit));
      Prev_Unit : Iir;
      Old_Design_Unit : Iir_Design_Unit;
      Implicit : Implicit_Signal_Declaration_Type;
   begin
      --  Sanity check: can analyze either previously analyzed unit or just
      --  parsed unit.
      case Get_Date (Design_Unit) is
         when Date_Parsed =>
            Set_Date (Design_Unit, Date_Analyzing);
         when Date_Valid =>
            null;
         when Date_Obsolete =>
            --  This happens only when design files are added into the library
            --  and keeping obsolete units (eg: to pretty print a file).
            Set_Date (Design_Unit, Date_Analyzing);
         when others =>
            raise Internal_Error;
      end case;

      --  If there is already a unit with the same name, mark it as being
      --  replaced.
      if Library_Unit /= Null_Iir then
         if Get_Kind (Library_Unit) in Iir_Kinds_Primary_Unit then
            Prev_Unit := Libraries.Find_Primary_Unit
              (Library, Get_Identifier (Library_Unit));
            if Is_Valid (Prev_Unit) and then Prev_Unit /= Design_Unit then
               Set_Date (Prev_Unit, Date_Replacing);
            end if;
         end if;
      end if;

      --  Save and set current_design_unit.
      Old_Design_Unit := Current_Design_Unit;
      Current_Design_Unit := Design_Unit;
      Push_Signals_Declarative_Part (Implicit, Null_Iir);

      --  Have a clean and empty state for scopes.
      Push_Interpretations;

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

      --  LRM 11.2
      --  Every design unit is assumed to contain the following implicit
      --  context items as part of its context clause:
      --    library STD, WORK; use STD.STANDARD.all;
      Sem_Scopes.Add_Name (Libraries.Std_Library, Std_Names.Name_Std, False);
      Sem_Scopes.Add_Name (Library, Std_Names.Name_Work, False);
      Sem_Scopes.Use_All_Names (Standard_Package);

      --  Use pre-defined locations for STD and WORK library (as they may
      --  be later overriden).
      Set_Location (Libraries.Std_Library, Libraries.Library_Location);
      Set_Location (Library, Libraries.Library_Location);

      if Get_Dependence_List (Design_Unit) = Null_Iir_List then
         Set_Dependence_List (Design_Unit, Create_Iir_List);
      end if;
      Add_Dependence (Std_Standard_Unit);

      --  Analyze context clauses.
      Sem_Context_Clauses (Design_Unit);

      --  Analyze the library unit.
      if Library_Unit /= Null_Iir then
         --  Can be null_iir in case of parse error.
         case Iir_Kinds_Library_Unit (Get_Kind (Library_Unit)) is
            when Iir_Kind_Entity_Declaration =>
               Sem_Entity_Declaration (Library_Unit);
            when Iir_Kind_Architecture_Body =>
               Sem_Architecture_Body (Library_Unit);
            when Iir_Kind_Package_Declaration =>
               Sem_Package_Declaration (Library_Unit);
            when Iir_Kind_Package_Body =>
               Sem_Package_Body (Library_Unit);
            when Iir_Kind_Configuration_Declaration =>
               Sem_Configuration_Declaration (Library_Unit);
            when Iir_Kind_Package_Instantiation_Declaration =>
               Sem_Package_Instantiation_Declaration (Library_Unit);
            when Iir_Kind_Context_Declaration =>
               Sem_Context_Declaration (Library_Unit);
            when Iir_Kinds_Verification_Unit =>
               Sem_Psl.Sem_Psl_Verification_Unit (Library_Unit);
            when Iir_Kind_Foreign_Module =>
               raise Internal_Error;
         end case;
      end if;

      Close_Declarative_Region;

      Pop_Interpretations;

      if Get_Date (Design_Unit) = Date_Analyzing then
         Set_Date (Design_Unit, Date_Analyzed);
      end if;

      if Get_Analysis_Checks_List (Design_Unit) /= Null_Iir_List then
         Sem_Analysis_Checks_List (Design_Unit, False);
      end if;

      --  Restore current_design_unit.
      Current_Design_Unit := Old_Design_Unit;
      Pop_Signals_Declarative_Part (Implicit);
   end Semantic;
end Vhdl.Sem;
