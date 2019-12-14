entity tb_snum04 is
end tb_snum04;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_snum04 is
  signal r : boolean;
begin
  cmp04_1: entity work.snum04
    port map (r);

  process
  begin
    wait for 1 ns;
    assert r severity failure;

    wait;
  end process;
end behav;
