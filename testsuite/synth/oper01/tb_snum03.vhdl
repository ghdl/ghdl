entity tb_snum03 is
end tb_snum03;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_snum03 is
  signal r : boolean;
begin
  cmp03_1: entity work.snum03
    port map (r);

  process
  begin
    wait for 1 ns;
    assert r severity failure;

    wait;
  end process;
end behav;
