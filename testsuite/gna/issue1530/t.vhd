library ieee;
use ieee.std_logic_1164.all;

entity t is
end entity;

architecture a of t is
  signal clk : std_logic := '0';
  signal x   : std_logic := '0';
begin
  clk <= not clk after 5 ns;

  process
  begin
    wait for 12 ns;
    x <= '1';
    wait for 100 ns;
    std.env.finish;
  end process;

  default clock is rising_edge(clk);
  A1 : assert always stable(x) report "x changed";
end architecture;
