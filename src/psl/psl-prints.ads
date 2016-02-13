--  PSL - Pretty print
--  Copyright (C) 2002-2016 Tristan Gingold
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

with PSL.Nodes; use PSL.Nodes;
with PSL.Priorities; use PSL.Priorities;

package PSL.Prints is
   procedure Print_Unit (Unit : Node);
   procedure Print_Sequence
     (Seq : Node; Parent_Prio : Priority := Prio_Lowest);
   procedure Print_Property
     (Prop : Node; Parent_Prio : Priority := Prio_Lowest);
   procedure Print_Expr (N : Node; Parent_Prio : Priority := Prio_Lowest);

   --  Procedure to display HDL_Expr nodes.
   type HDL_Expr_Printer_Acc is access procedure (N : HDL_Node);
   HDL_Expr_Printer : HDL_Expr_Printer_Acc;

   procedure Print_HDL_Expr (N : HDL_Node);

   --  Like Print_Expr but always put parenthesis.
   procedure Dump_Expr (N : Node);

end PSL.Prints;
