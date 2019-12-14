entity tb_snum02 is
end tb_snum02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_snum02 is
  signal r : boolean;
begin
  cmp02_1: entity work.snum02
    port map (r);

  process
  begin
    wait for 1 ns;
    assert r severity failure;

    wait;
  end process;
end behav;
