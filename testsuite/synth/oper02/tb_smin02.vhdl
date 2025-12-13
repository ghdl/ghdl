entity tb_smin02 is
end;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_smin02 is
  signal l, res : signed (3 downto 0);
  signal r : integer;
begin
  min01_1: entity work.smin02
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
    assert res = "1011" severity failure;

    wait;
  end process;
end behav;
