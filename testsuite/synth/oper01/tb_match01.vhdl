entity tb_match01 is
end tb_match01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_match01 is
  signal l  : std_ulogic_vector(11 downto 0);
  signal r  : boolean;
begin
  match01_1: entity work.match01
    port map (l, r);

  process
  begin
    l <= x"f00";
    wait for 1 ns;
    assert r severity failure;

    l <= x"ff0";
    wait for 1 ns;
    assert r severity failure;

    l <= x"ef0";
    wait for 1 ns;
    assert not r severity failure;

    wait;
  end process;
end behav;
