entity tb_targ02 is
end tb_targ02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_targ02 is
  signal o0 : std_logic;
  signal o1 : std_logic;
  signal o2 : std_logic;
begin
  dut: entity work.targ02
    port map (o0, o1, o2);

  process
  begin
    wait for 1 ns;
    assert o2 = '0' and o1 = '0' and o0 = '1' severity failure;

    wait;
  end process;
end behav;
