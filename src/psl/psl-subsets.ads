--  PSL - Simple subset
--  Copyright (C) 2002-2016 Tristan Gingold
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

   --  Return True iff N is an async abort.
   --  True for N_Async_Abort, False for N_Sync_Abort.
   --  Here we also decide for N_Abort.
   function Is_Async_Abort (N : Node) return Boolean;
end PSL.Subsets;
