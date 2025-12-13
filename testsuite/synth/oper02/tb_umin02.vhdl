entity tb_umin02 is
end;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_umin02 is
  signal l, res : unsigned (3 downto 0);
  signal r : natural;
begin
  min01_1: entity work.umin02
    port map (
      a  => l,
      b  => r,
      o => res);

  process
  begin
    l <= "0111";
    r <= 2;
    wait for 1 ns;
    assert res = "0010" severity failure;

    l <= "1011";
    r <= 6;
    wait for 1 ns;
    assert res = "0110" severity failure;

    l <= "0011";
    r <= 4;
    wait for 1 ns;
    assert res = "0011" severity failure;

    wait;
  end process;
end behav;
