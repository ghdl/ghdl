entity tb_umin01 is
end;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_umin01 is
  signal l, r, res : unsigned (3 downto 0);
begin
  min01_1: entity work.umin01
    port map (
      a  => l,
      b  => r,
      o => res);

  process
  begin
    l <= "0111";
    r <= "0010";
    wait for 1 ns;
    assert res = "0010" severity failure;

    l <= "1011";
    r <= "0110";
    wait for 1 ns;
    assert res = "0110" severity failure;

    wait;
  end process;
end behav;
