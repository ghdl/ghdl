entity tb_top is
end tb_top;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_top is
  signal p : std_logic;
  signal q : std_logic;
begin
  dut: entity work.top
    port map (p, q);

  process
  begin
    wait for 1 ns;
    assert p = '0' severity failure;
    assert q = '1' severity failure;
    wait;
  end process;
end behav;
