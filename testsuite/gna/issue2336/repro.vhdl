library ieee;
use ieee.std_logic_1164.all;

entity repro is
  port (a, b, clk : std_logic);
end;

architecture behav of repro is
begin
  default clock is rising_edge(clk);

   -- This assertion should hold, but doesn't (GHDL BUG)
--  NEXT_0_a : assert always (a -> next_event_e(true)[2 to 2] (b));
--    NEXT_1_a : assert always (a -> next_e[2 to 4] (b));
--    NEXT_2_a : assert always (a -> {true[->2 to 3] : b});
  NEXT_3_a : assert always (a -> {true[->2 to 3] ; b});
end behav;
