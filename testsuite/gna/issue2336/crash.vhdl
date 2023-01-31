library ieee;
use ieee.std_logic_1164.all;

entity repro is
  port (a, b, clk : std_logic);
end;

architecture behav of repro is
begin
  default clock is rising_edge(clk);

   -- This assertion should hold, but doesn't (GHDL BUG)
  NEXT_0_a : assert always (a -> next_event_e('1')[1 to 1] (b))
    report "NEXT_0_a failed";
end behav;
