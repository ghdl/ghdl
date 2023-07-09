--  Verilog semantic analyzer (scopes)
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

with Types; use Types;
with Verilog.Nodes; use Verilog.Nodes;

package Verilog.Sem_Scopes is
   --  There are several references to name spaces and scopes in the standard:
   --  3.13 Name spaces
   --  23.9 Scope rules

   --  1800-2017 3.13 Name spaces
   --  a) The definitions name space unifies all the non-nested module,
   --     primitive, program and interface identifiers defined outside all
   --     other declarations.
   procedure Add_Definition (Decl : Node);
   function Get_Definition (Id : Name_Id) return Node;

   --  1800-2017 3.13 Name spaces
   --  b) The package name space unifies all the package identifiers defined
   --     among all compilation units.
   procedure Add_Package (Decl : Node);
   function Get_Package (Id : Name_Id) return Node;

   --  1800-2017 3.13 Name spaces
   --  c) The compilation-unit scope name space exists outside the module,
   --     interface, package, checker, program and primitive constructs.
   --  GHDL: this is considered as a scope.

   --  1800-2017 3.13 Name spaces
   --  d) The test macro name space is global within the compilation unit.
   --  GHDL: this is handled by the scanner.

   --  1800-2017 3.13 Name spaces
   --  e) The module name space is introduced by the module, interface,
   --     package, program, checker and primitive constructs.
   --  GHDL: this is considered as a scope.

   --  1800-2017 3.13 Name spaces
   --  f) The block name space is introduced by named or unnamed blocks, the
   --     specify, function and task constructs.
   --  GHDL: this is considered as a scope.

   --  1800-2017 3.13 Name spaces
   --  g) The port name space is introduced by the module, interface,
   --     primitive, and program constructs.
   --  GHDL: a scope is created to check for duplicate definition.  Names are
   --     referenced at well known places.

   --  1800-2017 3.13 Name spaces
   --  h) The attribute name space is enclosed by the (* and *) constructs
   --     attached to a language element.
   --  GHDL: todo.

   --  1800-2017 23.9 Scope rules
   --  The following elements define a new scope in SystemVerilog:
   --  - Modules
   --  - Interfaces
   --  - Programs
   --  - Checkers
   --  - Packages
   --  - Classes
   --  - Tasks
   --  - Functions
   --  - begin-end blocks (named or unnamed)
   --  - fork-join blocks (named or unnamed)
   --  - Generate blocks
   -- GHDL: What is the difference between a scope and a name-space ?
   --   Certains constructs define a scope (class) but not a name-space, some
   --   create a name-space (specify) but not a scope!

   --  Open and close name spaces.
   procedure Open_Name_Space;
   procedure Close_Name_Space;

   --  Add declaration N in the current name space.  Emit an error if the
   --  name is already declared in the name space.
   procedure Add_Decl (N : Node; Potentially : Boolean := False);
   procedure Replace_Decl (N : Node);

   --  Call Add_Decl for each element of CHAIN.
   procedure Add_Decl_Chain (Chain : Node; Potentially : Boolean := False);

   --  Call Add_Decl for all functions and tasks of CHAIN (except external
   --  ones).  This is to follow 1800-2017 23.8.1 Task and function name
   --  resolution.
   procedure Add_Tf_Decls (Chain : Node);

   --  Add declarations of ATYPE (useful for enums).
   procedure Add_Item_Type_Decl (Atype : Node);

   --  Call Add_Decl for items of Klass.
   procedure Add_Class_Decls (Klass : Node);

   --  Implement import PKG::*
   procedure Wildcard_Import (Pkg : Node);

   --  Return the corresponding declaration for identifier ID.
   --  Return NULL_NODE if ID is not declared.
   function Get_Decl (Id : Name_Id) return Node;

   --  Like Get_Decl, but do not import a name from a package.
   --  This is used by wildcard connections (.*).
   function Get_Decl_No_Import (Id : Name_Id) return Node;

   --  Return the current declaration of ID.  Return Null_Node if no
   --  declaration or not in the current scope.
   function Peek_Scope_Decl (Id : Name_Id) return Node;

   procedure Init;
end Verilog.Sem_Scopes;
