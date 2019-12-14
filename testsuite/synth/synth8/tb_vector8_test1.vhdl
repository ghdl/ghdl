entity tb_vector8_test1 is
end tb_vector8_test1;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_vector8_test1 is
  signal r : std_logic;
begin
  dut: entity work.vector8_test1
    port map (r);

  process
  begin
    wait for 1 ns;
    assert r = '1' severity failure;
    wait;
  end process;
end behav;
