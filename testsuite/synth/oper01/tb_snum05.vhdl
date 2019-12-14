entity tb_snum05 is
end tb_snum05;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_snum05 is
  signal r : boolean;
begin
  cmp05_1: entity work.snum05
    port map (r);

  process
  begin
    wait for 1 ns;
    assert r severity failure;

    wait;
  end process;
end behav;
