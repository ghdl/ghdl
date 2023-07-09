--  Verilog name resolution (first pass)
--  Copyright (C) 2023 Tristan Gingold
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
--  along with this program.  If not, see <gnu.org/licenses>.

with Verilog.Nodes; use Verilog.Nodes;

package Verilog.Resolve_Names is
   --  Names resolution.
   --  See Sem_Names too.
   --
   --  This is quite complex because of typedef, class and type parameters.
   --
   --  The first pass [resolve] resolves names, except:
   --  - within classes.  Because inheritance defines additional names that
   --    are visible within classes, so the base class name must be resolved
   --    before.  But it can resolve to a typedef whose full declaration
   --    appear after the class.
   --  - hierarchical names.  Hierarchy is not yet built.
   --
   --  Second pass: analyze params, types, declarations and possibly constant
   --  functions.
   --
   --  Third pass: task and function bodies, statements, expressions...
   --
   --  However:
   --  - Hierarchical names and tf names are resolved during full analysis.
   --  - Implicit and wildcard names cannot be resolved here as the module
   --    name may not be resolved (they are resolved during elaboration due
   --    to config).
   --  - What about classes ?  Resolution of names within a class depends on
   --    the base class, which can be a type parameter.

   --  Partially resolve names of ITEMS.  This the the first analysis pass.
   --  Why partially ?
   --  - task/function names should be searched in the instantiation tree.
   --  - hierarchical names cannot be resolved before instantiation.
   --  - class items are not resolved.  A class can inherit from another
   --    class, which may be just a typedef, or which has to be instantiated,
   --    or which is a type parameter.
   procedure Resolve_Names_Compilation_Unit (Cu : Node);

   --  Resolve names in class KLASS using inheritance.
   procedure Resolve_Names_Class_Complete (Klass : Node);

   --  Resolve names in subroutine RTN.
   procedure Resolve_Names_Subroutine_Body (Rtn : Node);

   --  Resolve wildcards.
   procedure Resolve_Wildcard_Connections_Chain (Chain : Node);

   --  Create implicit net for port PORT.
   procedure Create_Implicit_Net_For_Port_Declaration
     (Port : Node; Net : out Node);
end Verilog.Resolve_Names;
