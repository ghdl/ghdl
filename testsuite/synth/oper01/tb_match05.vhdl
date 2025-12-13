entity tb_match05 is
end tb_match05;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_match05 is
  signal l  : std_ulogic_vector(11 downto 0);
  signal r  : boolean;
begin
  match01_1: entity work.match05
    port map (l, r);

  process
  begin
    l <= x"f00";
    wait for 1 ns;
    assert not r severity failure;

    l <= x"ff0";
    wait for 1 ns;
    assert not r severity failure;

    l <= x"ef0";
    wait for 1 ns;
    assert not r severity failure;

    wait;
  end process;
end behav;
