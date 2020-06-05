library ieee;
use ieee.std_logic_1164.all;

entity psl_prev is
  port (clk, a, b : std_logic);
end entity psl_prev;


architecture psl of psl_prev is
begin
  -- All is sensitive to rising edge of clk
  default clock is rising_edge(clk);

  -- This assertion should fail
  stable_1 : assert always (a -> prev(b) = b);
end architecture psl;
