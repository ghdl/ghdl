library ieee;
use ieee.std_logic_1164.all;

entity top5 is
end entity;

architecture a of top5 is
    signal a,b,c,d : std_logic := '0';
    signal clk_sys, clk1, clk2 : std_logic;
begin
  -- psl default clock is clk_sys;

  -- Following error is thrown:
  -- "top.vhd:43:58:error: left-hand side operand of logical '->' must be
  --  a boolean"
  -- This is not true, LHS and RHS of implication shall be FL_Property, not
  -- just boolean (6.2.1.6.1)
  -- psl my_seq : assert always (a -> next b -> next c -> next d);
end;
