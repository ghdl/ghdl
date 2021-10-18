entity tb_uns02 is
end tb_uns02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_uns02 is
  signal r : boolean;
begin
  cmp01_1: entity work.uns02
    port map (r);

  process
  begin
    wait for 1 ns;
    assert r severity failure;

    wait;
  end process;
end behav;
