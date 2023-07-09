--  Verilog semantic analyzer (expressions)
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

package Verilog.Sem_Expr is
   --  Expression data type.
   --  One interesting aspect of the verilog type system is how the type is
   --  determined.  In verilog, the size of the arrays are known at compile
   --  time.  The rules are (incompletly) explained in IEEE1364 2005 5.4
   --  Expression bit lengths.  Likewise, the sign is also known at compile
   --  time.  Length is determined in two passes.  First Sem_Sub_Expression
   --  analyze expressions from the bottom to the top and set the type as
   --  determined by sub expressions.  Then Sem_Propagate_Length set the length
   --  (by modifying the type) from the top to the bottom and possibly also
   --  insert convertions (size extension, truncation...).  Note that both
   --  passes are intermixed: Sem_Propagate_Length is called during
   --  Sem_Sub_Expression for self-determined operands.

   function Sem_Sub_Expression (Expr : Node; Etype : Node) return Node;
   function Sem_Propagate_Length (Expr : Node; Etype : Node) return Node;

   function Sem_Binary_Expression_Type (Lt : Node; Rt : Node) return Node;

   function Sem_Expression (Expr : Node; Etype : Node) return Node;
   procedure Sem_Cond_Expression (Stmt : Node);

   function Sem_Event_Expression (Expr : Node) return Node;

   procedure Sem_Streaming_Concatenation (Expr : Node);

   --  Convert EXPR to type RTYPE.  Does not perform error checks, use
   --  Insert_Assignment_Compatible if needed.
   function Implicit_Conversion (Expr : Node; Rtype : Node) return Node;

   --  Analyze $signed/$unsigned.
   procedure Sem_Sign_System_Function_Call (Call : Node; Is_Signed : Boolean);

   --  Analyze $cast
   procedure Sem_Cast_System_Function_Call (Call : Node);

   --  Analyze $typename
   procedure Sem_Typename_System_Function_Call (Call : Node);

   --  Analyze $left, $right, $low, $high or $size.
   procedure Sem_Array_Dimension_System_Function_Call (Call : Node);

   procedure Sem_Subroutine_Call_Name (Call : Node);
   function Sem_Subroutine_Call_Suffix (Call : Node) return Node;
   procedure Sem_Array_Method_Call_With (Call : Node);

   function Build_Error_Expr (Orig : Node) return Node;
end Verilog.Sem_Expr;
