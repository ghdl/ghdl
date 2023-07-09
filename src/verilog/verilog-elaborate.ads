--  Verilog elaboration
--  Copyright (C) 2023 Tristan Gingold
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
with Verilog.Nodes; use Verilog.Nodes;

package Verilog.Elaborate is
   --  Elaborate a design and return the root module.
   --
   --  See:
   --  1800-2017 23.10.4 Elaboration considerations
   --
   --  The compilation units must have been analyzed.  This means (almost) full
   --  analysis of CU-level items and packages, (almost) full names resolution.
   --
   --  The set of root units is computed.
   --
   --  The design hierarchy is instantiated (until the generate blocks).
   --
   --  Parameters are overriden.
   --
   --  Design hierarchy is analyzed.
   --   Declarations
   --   Then statements/expressions (needed due to hierarchical names).
   --   (Note: according to 6.18 User-defined types, hierarchical references
   --    to type_identifier shall not be allowed.)
   --
   --  Continue until there is no more generate blocks.
   function Elab_Design (Top : Node := Null_Node) return Node;

   --  Sub procedures.

   --  Create the root module.  It is empty.
   function Create_Root_Module return Node;

   --  Create an instantiation for DECL, to be put into the root module.
   function Create_Root_Instance (Decl : Node; Loc : Location_Type)
                                 return Node;

   --  Resolve all instantiations, mark instantiated modules.
   procedure Resolve_Instantiations (Units : Node);

   --  Chain of compilation unit (source files).
   Units_Chain : Node := Null_Node;
end Verilog.Elaborate;
