entity tb_string01 is
end tb_string01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_string01 is
  signal a : std_logic;
begin
  dut: entity work.string01
    port map (a);

  process
  begin
    wait for 1 ns;
    assert a = '1' severity failure;

    wait;
  end process;
end behav;
