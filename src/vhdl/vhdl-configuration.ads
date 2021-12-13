--  Configuration generation.
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
with Tables;

package Vhdl.Configuration is
   package Design_Units is new Tables
     (Table_Component_Type => Iir_Design_Unit,
      Table_Index_Type => Natural,
      Table_Low_Bound => 1,
      Table_Initial => 16);

   --  Get the top configuration to build a design hierarchy whose top is
   --  PRIMARY + SECONDARY.
   --  PRIMARY must designate a configuration declaration or an entity
   --  declaration.  In the last case, SECONDARY must be null_identifier or
   --  designates an architecture declaration.
   --
   --  creates a list of design unit.
   --  and return the top configuration.
   --  Note: this set the Elab_Flag on units.
   function Configure
     (Library_Id : Name_Id; Primary_Id : Name_Id; Secondary_Id : Name_Id)
     return Iir;

   --  Add design unit UNIT (with its dependences) in the design_units table.
   procedure Add_Design_Unit (Unit : Iir_Design_Unit; From : Location_Type);

   --  Add all vunits that are bound to any configured entity architecture.
   procedure Add_Verification_Units;

   --  If set, all design units (even package bodies) are loaded.
   Flag_Load_All_Design_Units : Boolean := True;

   --  If set, compute the File_Dependence_List of design files.
   Flag_Build_File_Dependence : Boolean := False;

   --  Check if ENTITY can be at the top of a hierarchy, ie:
   --  ENTITY has no generics or all generics have a default expression
   --  ENTITY has no ports or all ports type are constrained.
   --  If not, emit a elab error message.
   procedure Check_Entity_Declaration_Top
     (Entity : Iir_Entity_Declaration; Enable_Override : Boolean);

   --  Use heuritics to find the top entity in FROM (either a library or
   --  a design file): mark all instantiated units and return the unmarked
   --  one if there is only one.
   --  LOC is used to report errors.
   function Find_Top_Entity (From : Iir; Loc : Location_Type) return Iir;

   --  Hook for Find_Top_Entity to deal with foreign units.
   --  When called for a foreign module N, the procedure must walk N to find
   --  all the module instantiations.  For each instantiation, it must look
   --  for the definition in the VHDL scope table and set the Elab flag.
   type Mark_Instantiated_Units_Access is access procedure (N : Int32);
   Mark_Foreign_Module : Mark_Instantiated_Units_Access;

   type Apply_Foreign_Override_Access is access procedure
     (Top : Int32; Gen : String; Value : String);
   Apply_Foreign_Override : Apply_Foreign_Override_Access;

   --  Add an override for generic ID.
   procedure Add_Generic_Override (Name : String; Value : String);

   --  Apply generic overrides to entity ENT.
   procedure Apply_Generic_Override (Ent : Iir);
end Vhdl.Configuration;
