entity tb_max01 is
end tb_max01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_max01 is
  signal l, r  : natural;
  signal res : natural;
begin
  max01_1: entity work.max01
    port map (
      a  => l,
      b  => r,
      o => res);

  process
  begin
    l <= 12;
    r <= 15;
    wait for 1 ns;
    assert res = 15 severity failure;

    wait;
  end process;
end behav;
