library ieee;
use ieee.std_logic_1164.all;

entity top4 is
end entity;

architecture a of top4 is
    signal a,b,c,d : std_logic := '0';
    signal clk_sys, clk1, clk2 : std_logic;
begin
  -- psl default clock is clk_sys;

  -- Following error is thrown:
  --  "top.vhd:43:28:error: operand of a 'never' operator must be a boolean
  --   or a sequence"
  -- While for "assert always", this works. IMHO this should work for both,
  -- equally since productions for never|always are the same.
  -- It is not true that operand of never must be a boolean or a sequence.
  -- It can be FL_property or sequence, which is much wider set.
  -- psl my_seq : assert never (a) -> eventually! (b);
end;
