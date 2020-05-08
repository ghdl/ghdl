library ieee;
use ieee.std_logic_1164.all;


entity issue is
end issue;


architecture sim of issue is

  signal clk  : std_logic := '1';
  signal a, b : std_logic := '0';

begin

  clk <= not clk after 5 ns;

  a <= '1' after 20 ns,
       '0' after 30 ns,
       '1' after 40 ns,
       '0' after 50 ns;

  b <= '1' after 50 ns,
       '0' after 60 ns,
       '1' after 70 ns,
       '0' after 80 ns;

  -- All is sensitive to rising edge of clk
  -- psl default clock is rising_edge(clk);

  -- This assertion holds
  -- psl NEXT_0_a : assert always (a -> next_e[3 to 5] (b));

end architecture sim;
