entity tb_case05 is
end;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_case05 is
  signal a : integer range -3 to 2;
  signal o : std_logic;
begin
  dut: entity work.case05
    port map (a, o);

  process
  begin
    a <= -3;
    wait for 1 ns;
    assert o = '0';

    a <= -2;
    wait for 1 ns;
    assert o = '0';

    a <= -1;
    wait for 1 ns;
    assert o = '1';

    a <= 0;
    wait for 1 ns;
    assert o = '1';

    a <= 1;
    wait for 1 ns;
    assert o = '0';

    a <= 2;
    wait for 1 ns;
    assert o = '0';

    wait;
  end process;
end behav;
