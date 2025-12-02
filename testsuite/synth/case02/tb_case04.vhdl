entity tb_case04 is
end;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_case04 is
  signal a : natural range 0 to 3;
  signal o : std_logic;
begin
  dut: entity work.case04
    port map (a, o);

  process
  begin
    a <= 0;
    wait for 1 ns;
    assert o = '0';

    a <= 1;
    wait for 1 ns;
    assert o = '1';

    a <= 2;
    wait for 1 ns;
    assert o = '0';

    wait;
  end process;
end behav;
