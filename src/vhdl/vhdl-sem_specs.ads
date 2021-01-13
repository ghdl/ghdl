--  Semantic analysis.
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
with Types; use Types;
with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Tokens;

package Vhdl.Sem_Specs is
   --  Return the attribute_value for named entity ENT and attribute identifier
   --  ID.  Return Null_Iir if ENT was not decorated with attribute ID.
   function Find_Attribute_Value (Ent : Iir; Id : Name_Id) return Iir;

   --  Return the node containing the attribute_value_chain field for DECL.
   --  This is the parent of the attribute specification, so in general this
   --  is also the parent of the declaration, but there are exceptions...
   function Get_Attribute_Value_Chain_Parent (Decl : Iir) return Iir;

   function Get_Entity_Class_Kind (Decl : Iir) return Vhdl.Tokens.Token_Type;

   procedure Sem_Attribute_Specification (Spec : Iir_Attribute_Specification);

   --  Check declarations following an ALL/OTHERS attribute specification.
   --  ATTR_SPEC_CHAIN is the linked list of all attribute specifications whith
   --  the entity name list ALL or OTHERS until the current declaration DECL.
   --  So no specification in the chain must match the declaration.
   procedure Check_Post_Attribute_Specification
     (Attr_Spec_Chain : Iir; Decl : Iir);

   procedure Sem_Disconnection_Specification
     (Dis : Iir_Disconnection_Specification);

   procedure Sem_Step_Limit_Specification (Limit : Iir);

   procedure Sem_Configuration_Specification
     (Parent_Stmts : Iir; Conf : Iir_Configuration_Specification);

   --  Analyze binding indication BIND of configuration specification or
   --  component configuration PARENT.
   --  PRIMARY_BINDING is not Null_Iir for an incremental binding.
   procedure Sem_Binding_Indication (Bind : Iir_Binding_Indication;
                                     Parent : Iir;
                                     Primary_Binding : Iir);

   --  Analyze entity aspect ASPECT and return the entity declaration.
   --  Return NULL_IIR if not found.
   function Sem_Entity_Aspect (Aspect : Iir) return Iir;

   --  Analyze component_configuration or configuration_specification SPEC.
   --  STMTS is the concurrent statement list related to SPEC.
   procedure Sem_Component_Specification
     (Parent_Stmts : Iir; Spec : Iir; Primary_Binding : out Iir);

   --  Check that all interfaces of INTER_CHAIN are associated either by
   --  ASSOC1 or ASSOC2 (if not null_iir) when they need to be associated.
   procedure Sem_Check_Missing_Generic_Association
     (Inter_Chain : Iir;  Assoc1 : Iir; Assoc2 : Iir; Loc : Iir);

   --  Create a default binding indication for component COMP which will be
   --  bound with entity ENTITY_UNIT.
   --  If ENTITY_UNIT is NULL_IIR, the component is not bound.
   --  If FORCE is True, a binding indication will be created even if the
   --   component is not bound (this is an open binding indication).
   --  If CREATE_MAP_ASPECT is true, port and generic map aspect are created.
   --  PARENT is used to report error.
   function Sem_Create_Default_Binding_Indication
     (Comp : Iir_Component_Declaration;
      Entity_Unit : Iir_Design_Unit;
      Parent : Iir;
      Force : Boolean;
      Create_Map_Aspect : Boolean)
     return Iir_Binding_Indication;

   --  Create a default generic or port map aspect that associates all elements
   --  of ENTITY (if any) to elements of COMP with the same name or to
   --  an open association.
   --  If KIND is GENERIC_MAP, apply this on generics, if KIND is PORT_MAP,
   --  apply this on ports.
   --  PARENT is used to report errors.
   type Map_Kind_Type is (Map_Generic, Map_Port);
   function Create_Default_Map_Aspect
     (Comp : Iir; Entity : Iir; Kind : Map_Kind_Type; Parent : Iir)
     return Iir;

   --  Explain why there is no default binding for COMP.
   procedure Explain_No_Visible_Entity (Comp: Iir_Component_Declaration);

   function Get_Visible_Entity_Declaration (Comp: Iir_Component_Declaration)
                                           return Iir_Design_Unit;

   procedure Sem_Specification_Chain (Decls_Parent : Iir; Parent_Stmts: Iir);
end Vhdl.Sem_Specs;
