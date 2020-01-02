entity tb_test is
end tb_test;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_test is
  signal a : std_logic;
  signal b : std_logic;
begin
  dut: entity work.test
    port map (a, b);

  process
  begin
    wait for 1 ns;
    assert b = '0' severity failure;
    wait;
  end process;
end behav;
