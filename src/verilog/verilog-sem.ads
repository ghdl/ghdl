--  Verilog semantic analyzer
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

package Verilog.Sem is
   --  Disable warning for unconnected ports
   Flag_Synthesis : Boolean := False;

   procedure Sem_Block_Items_Declaration (Items : Node);
   procedure Sem_Var (Var : Node);

   procedure Sem_Class_Type_Methods (Klass : Node);

   --  Analyze a single compilation unit (ie a file) just after parser.
   --  It should analyze as much as possible, so that as many errors can
   --  be detected.  But it is not possible to do a full analysis before
   --  elaboration (because for example of hierarchical names).
   --
   --  1800-2017 3.12 Compilation and elaboration
   --  This standard does not normally specify requirements regarding the
   --  order of compilation for design elements.  The two exceptions are the
   --  rules regarding "compilation units" where actual file boundaries during
   --  compilation are significant, and the rules regarding references to
   --  package items where the compilation of a package is required to precede
   --  references to it.
   --
   --  Resolve names (may require two passes, may create implicit net for
   --   ports or for continuous assign)
   --  Analyze packages and top-level class, vars, subroutines, typedef.
   --  Only ports of modules and interfaces are slightly analyzed:
   --   redeclarations are moved to items list.
   --
   --  TODO: in case of elaboration, move this phase after parsing all the
   --   source files and resolving module instantiation names, in order to
   --   expand wildcard connections.
   procedure Sem_Compilation_Unit (Source : Node);

   --  Analyze a constructed design.  Called by Verilog.Elaborate.Elab_Design.
   --  1. Apply parameters override and parameter value assignments.
   --  2. Analyze declarations (types of ports, nets, variables; parameters;
   --     typedef), including instances.
   --  3. Expand wildcard connections
   --  4. Analyze instantiated modules
   --  4.1. Ports default value
   --  4.2. Module items
   --  4.3. Net declaration assignments
   --  4.4. Subroutine bodies
   --  4.5. Instances
   procedure Sem_Design (Root : Node);
end Verilog.Sem;
