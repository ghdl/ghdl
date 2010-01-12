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
