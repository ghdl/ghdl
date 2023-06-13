--  Verilog parser
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

package Verilog.Parse is
   --  Return an N_Source_Text.
   function Parse_File (Sfe : Source_File_Entry) return Node;

   --  Simulation time unit (see 1800-2017 3.14.3 Simulation time unit).
   --  Unset_Simulation_Time_Unit is an (invalid) value used to mark the
   --  simulation time unit not yet set.
   --  Time unit is 10**Simulation_Time_Unit (so likely negative).
   Unset_Simulation_Time_Unit : constant Int32 := 10;
   Simulation_Time_Unit : Int32 := Unset_Simulation_Time_Unit;

   Default_Timescale : Node := Null_Node;

   procedure Update_Simulation_Time_Unit (Precision : Int32);

   --  Parsing ports:
   --  modules, programs, interfaces, subroutines have ports.  They can
   --  either be declared using the non-ANSI style or the ANSI style.
   --  See 1800-217 23.2.2.3 and Parse_Ports_List1 subprogram.
   --
   --  For the non-ANSI style, the ports list is a list of N_Port, containing
   --  an optional identifier and an optional expression.  The identifiers
   --  in the expression must be declared in the items list as a port
   --  declaration (N_Input, N_Output, N_Inout or N_Ref).
   --  The port_declaration can then be redeclared either as a net or as a
   --  variable.
   --
   --  For the ANSI style, the ports list is a list of ports declaration
   --  (N_Input, N_Output, ...) and the object (net or variable) is also
   --  created.  The type of the port is attached to the port declaration
   --  and the object is also attached to the port declaration through
   --  the Get/Set_Redeclaration field.

   --  Almost private, but used by debugger.

   --  Parse a procedural programming statement.
   function Parse_Statement (Parent : Node) return Node;

   --  Current scope.  According to VPI scope definition (2005), a scope is
   --  a module, a task, a function, a gen scope, a named begin or a named
   --  fork.  The purpose of the scope is only for VPI (and in particular for
   --  system function call).
   Current_Scope : Node;

end Verilog.Parse;
