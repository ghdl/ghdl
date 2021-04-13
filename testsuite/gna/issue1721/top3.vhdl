library ieee;
use ieee.std_logic_1164.all;

entity top3 is
end entity;

architecture a of top3 is
    signal a,b,c,d : std_logic := '0';
    signal clk_sys, clk1, clk2 : std_logic;
begin
  -- psl default clock is clk_sys;

  -- Following error is thrown: "translate_psl_expr: cannot handle N_IMP_BOOL"
  -- Combination of "never" + implication is not very usefull, since
  -- implication is always true apart from case where 1 -> 0, therefore it
  -- will mostly fail, however, it should not crash tool (the same goes for
  -- previous case too)
  -- psl my_seq : assert never (a -> b);
end;
