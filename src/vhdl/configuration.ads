--  Configuration generation.
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
with Types; use Types;
with Iirs; use Iirs;
with Tables;

package Configuration is
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
   function Configure (Primary_Id : Name_Id; Secondary_Id : Name_Id)
     return Iir;

   --  Likewise but directly from strings.
   function Configure (Primary : String; Secondary : String) return Iir;

   --  Add design unit UNIT (with its dependences) in the design_units table.
   procedure Add_Design_Unit (Unit : Iir_Design_Unit; From : Iir);

   --  If set, all design units (even package bodies) are loaded.
   Flag_Load_All_Design_Units : Boolean := True;

   --  If set, compute the File_Dependence_List of design files.
   Flag_Build_File_Dependence : Boolean := False;

   --  Check if ENTITY can be at the top of a hierarchy, ie:
   --  ENTITY has no generics or all generics have a default expression
   --  ENTITY has no ports or all ports type are constrained.
   --  If not, emit a elab error message.
   procedure Check_Entity_Declaration_Top (Entity : Iir_Entity_Declaration);

   --  Use heuritics to find the top entity in FROM (either a library or
   --  a design file): mark all instantiated units and return the unmarked
   --  one if there is only one.
   function Find_Top_Entity (From : Iir) return Iir;
end Configuration;
