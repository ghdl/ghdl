library ieee;
use ieee.std_logic_1164.all;

entity past_ent is
  port(
    clk : in std_logic
  );
end past_ent;

architecture expl_clock of past_ent is
  signal a : std_logic;
begin
  assert (always prev(a, 1) = '1') @rising_edge(clk); -- error
end expl_clock;

architecture default_clock of past_ent is
  signal a : std_logic;
  default clock is rising_edge(clk);
begin
  assert (always prev(a, 1) = '1'); -- ok
end default_clock;
