--  PSL - Simple subset
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

package PSL.Subsets is
   --  Check that N (a property) follows the simple subset rules from
   --  PSL v1.1 4.4.4 Simple subset.
   --  Ie:
   --  - The operand of a negation operator is a Boolean.
   --  - The operand of a 'never' operator is a Boolean or a Sequence.
   --  - The operand of an 'eventually!' operator is a Boolean or a Sequence.
   --  - The left-hand side operand of a logical 'and' operator is a Boolean.
   --  - The left-hand side operand of a logical 'or' operator is a Boolean.
   --  - The left-hand side operand of a logical implication '->' operator
   --    is a Boolean.
   --  - Both operands of a logical iff '<->' operator are Boolean.
   --  - The right-hand side operand of a non-overlapping 'until*' operator is
   --    a Boolean.
   --  - Both operands of an overlapping 'until*' operator are Boolean.
   --  - Both operands of a 'before*' operator are Boolean.
   --
   --  All other operators not mentioned above are supported in the simple
   --  subset without restriction.
   procedure Check_Simple (N : Node);
end PSL.Subsets;
