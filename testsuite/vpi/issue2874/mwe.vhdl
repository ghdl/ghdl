library ieee;
use ieee.std_logic_1164.all;

entity mwe is
end entity;

architecture test of mwe is
  signal clk : std_logic := '0';
begin
  clk <= not clk after 10 ns;

  process
  begin
    wait for 1 ns;
    report "should not execute" severity failure;
    wait;
  end process;
end architecture;
