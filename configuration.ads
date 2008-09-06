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
with GNAT.Table;

package Configuration is
   package Design_Units is new GNAT.Table
     (Table_Component_Type => Iir_Design_Unit,
      Table_Index_Type => Natural,
      Table_Low_Bound => 1,
      Table_Initial => 16,
      Table_Increment => 100);

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

   --  Add design unit UNIT (with its dependences) in the design_units table.
   procedure Add_Design_Unit (Unit : Iir_Design_Unit; From : Iir);

   --  If set, all design units (even package bodies) are loaded.
   Flag_Load_All_Design_Units : Boolean := True;

   Flag_Build_File_Dependence : Boolean := False;
end Configuration;
