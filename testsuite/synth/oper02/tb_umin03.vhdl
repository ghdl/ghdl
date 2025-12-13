entity tb_umin03 is
end;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_umin03 is
  signal r, res : unsigned (3 downto 0);
  signal l : natural;
begin
  min01_1: entity work.umin03
    port map (
      a  => l,
      b  => r,
      o => res);

  process
  begin
    l <= 2;
    r <= "0111";
    wait for 1 ns;
    assert res = "0010" severity failure;

    l <= 6;
    r <= "1011";
    wait for 1 ns;
    assert res = "0110" severity failure;

    l <= 4;
    r <= "0011";
    wait for 1 ns;
    assert res = "0011" severity failure;

    wait;
  end process;
end behav;
