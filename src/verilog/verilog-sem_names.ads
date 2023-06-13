--  Verilog semantic analyzer (names)
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

package Verilog.Sem_Names is
   --  Names analysis.

   --  Analyze LVAL as an lvalue.  Its type is determined by the name.
   --  Never returns Null_Node, but may return a node with a null type.
   --  Emit an error if constraints ALLOW_NET and ALLOW_VAR are not followed.
   function Sem_Lvalue (Lval : Node;
                        Allow_Net : Boolean := False;
                        Allow_Var : Boolean := False)
                       return Node;

   --  Likewise, but for a branch lvalue (AMS).
   function Sem_Branch_Lvalue (Lval : Node) return Node;

   --  Analyze NAME and return it or a different name.
   --  Set declaration on the prefix.  Set the type when applicable.
   --  In case of error, the declaration is not set.
   --  Never returns Null_Node.
   function Sem_Name (Name : Node) return Node;
   function Sem_Tf_Name (Name : Node) return Node;

   --  Analyze a scoped name.
   --  Used by sem_types when it needs to differenciate between a type and
   --  an expression.
   --  procedure Sem_Scoped_Name (Name : Node);

   --  Find the declaration of NAME in SCOPE (a class or a package).
   --  Return Null_Node if not found.
   function Find_Name_In_Scope (Scope : Node; Name : Node) return Node;
   function Find_Name_In_Decls (Items : Node; Name : Node) return Node;

   --  Find ID in an item chain.
   function Find_Id_In_Chain (Items : Node; Id : Name_Id) return Node;

   --  To be called when NAME has been resolved.
   --  Set the type of NAME if needed, and other flags.
   procedure Sem_Name_Declaration (Name : Node);
end Verilog.Sem_Names;
