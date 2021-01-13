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
with Vhdl.Nodes; use Vhdl.Nodes;
with Types; use Types;

package Vhdl.Sem is
   --  Semantic analysis for chapters 1, 2, 10 (uses clauses) and 11.

   -- Do the semantic analysis of design unit DESIGN_UNIT.
   -- Also add a few node or change some nodes, when for exemple an
   -- identifier is changed into an access to the type.
   procedure Semantic (Design_Unit: Iir_Design_Unit);

   -- Get the current design unit, ie, the parameter of the procedure semantic.
   function Get_Current_Design_Unit return Iir_Design_Unit;

   --  Makes the current design unit depends on UNIT.
   --  UNIT must be either an entity_aspect or a design_unit.
   procedure Add_Dependence (Unit : Iir);

   --  Add EL in the current design unit list of items to be checked later.
   procedure Add_Analysis_Checks_List (El : Iir);

   --  INTER_PARENT contains generics and ports interfaces;
   --  ASSOC_PARENT constains generics and ports map aspects.
   procedure Sem_Generic_Port_Association_Chain
     (Inter_Parent : Iir; Assoc_Parent : Iir);

   --  INTER_PARENT contains generics interfaces;
   --  ASSOC_PARENT constains generic aspects.
   procedure Sem_Generic_Association_Chain
     (Inter_Parent : Iir; Assoc_Parent : Iir);

   --  Return TRUE iff the actual of ASSOC can be the formal FORMAL.
   --  ASSOC must be an association_element_by_expression.
   function Can_Collapse_Signals (Assoc : Iir; Formal : Iir) return Boolean;

   --  Return TRUE iff LEFT and RIGHT are (in depth) equal.
   --  This corresponds to conformance rules, LRM 2.7
   function Are_Trees_Equal (Left, Right : Iir) return Boolean;

   --  Check requirements on number of interfaces for subprogram specification
   --  SUBPRG for a symbol operator ID.  Requirements only concern operators,
   --  and are defined in LRM 2.3.1.
   --  If ID is not an operator name, this subprogram does no checks.
   --  ID might be different from the identifier of SUBPRG when non object
   --  aliases are checked.
   procedure Check_Operator_Requirements (Id : Name_Id; Subprg : Iir);

   --  Analyze an use clause.
   --  This may adds use clauses to the chain.
   procedure Sem_Use_Clause (Clauses : Iir_Use_Clause);

   --  LRM 2.1  Subprogram Declarations.
   procedure Sem_Subprogram_Specification (Subprg : Iir);
   procedure Sem_Subprogram_Declaration (Subprg : Iir);

   --  LRM 2.2  Subprogram Bodies.
   procedure Sem_Subprogram_Body (Subprg : Iir);

   --  LRM08 4.4  Subprogram instantiation declarations
   procedure Sem_Subprogram_Instantiation_Declaration (Decl : Iir);

   --  LRM 2.5  Package Declarations.
   procedure Sem_Package_Declaration (Pkg : Iir_Package_Declaration);

   --  LRM 2.6  Package Bodies.
   procedure Sem_Package_Body (Decl : Iir);

   --  LRM08 4.9  Package Instantiation Declarations
   procedure Sem_Package_Instantiation_Declaration (Decl : Iir);

   --  Do late analysis checks (pure rules).
   procedure Sem_Analysis_Checks_List (Unit : Iir_Design_Unit;
                                       Emit_Warnings : Boolean);

   --  Analyze the uninstantiated package name of DECL, and return the
   --  package declaration.  Return Null_Iir if the name doesn't denote an
   --  uninstantiated package.
   function Sem_Uninstantiated_Package_Name (Decl : Iir) return Iir;

end Vhdl.Sem;
