entity tb_iassoc12 is
end tb_iassoc12;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_iassoc12 is
  signal a : natural;
  signal b : natural;
  signal v : natural;
begin
  dut: entity work.iassoc12
    port map (v, a, b);

  process
  begin
    v <= 5;
    wait for 1 ns;
    assert a = 6 severity failure;
    assert b = 7 severity failure;

    v <= 203;
    wait for 1 ns;
    assert a = 204 severity failure;
    assert b = 205 severity failure;

    wait;
  end process;
end behav;
