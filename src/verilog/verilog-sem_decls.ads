--  Verilog semantic analyzer (declarations)
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

package Verilog.Sem_Decls is
   --  Analyze type of declarations, typedef, parameters and type of classes.
   --  Has to deal with forward declarations.

   procedure Sem_Decl_Type (Decl : Node);
   procedure Sem_Decl_Type_Chain (Chain : Node);

   procedure Sem_Class_Type (Klass : Node);

   procedure Sem_Typedef_Type (Def : Node);

   --  For list of identifiers: they share the same type.
   --  Used by Sem_Types to analyze structure and union members.
   procedure Sem_Decl_List_Data_Type (Head : Node);

   --  Analyze the type of DECL.
   procedure Sem_Decl_Data_Type (Decl : Node);

   procedure Sem_Tf_Ports (Tf_Decl : Node);
end Verilog.Sem_Decls;
