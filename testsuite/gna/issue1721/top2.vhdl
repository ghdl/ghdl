library ieee;
use ieee.std_logic_1164.all;

entity top2 is
end entity;

architecture a of top2 is
    signal a,b,c,d : std_logic := '0';
    signal clk_sys, clk1, clk2 : std_logic;
begin
  -- psl default clock is clk_sys;

  -- Following throws bug occured with: "build_sere_fa: cannot handle N_IMP_SEQ"
  -- This is strange because with "always" this is working.
  -- According to PSL LRM 2003 FL_Property production for "always" is the
  -- same as for "never":
  --   FL_Property ::=
  --        always FL_Property
  --      | always Sequence
  --
  --   FL_Property ::=
  --        never FL_Property
  --      | never Sequence
  -- Therefore I think if sequence implication works with one, it shall work
  -- also with other.
  -- psl my_seq : assert never {a} |=> {b; c};
end a;
