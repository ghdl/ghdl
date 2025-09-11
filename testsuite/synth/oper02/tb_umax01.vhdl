entity tb_umax01 is
end tb_umax01;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_umax01 is
  signal l, r, res : unsigned(3 downto 0);
begin
  max01_1: entity work.umax01
    port map (
      a  => l,
      b  => r,
      o => res);

  process
  begin
    l <= "0111";
    r <= "0010";
    wait for 1 ns;
    assert res = "0111" severity failure;

    l <= "1011";
    r <= "0110";
    wait for 1 ns;
    assert res = "1011" severity failure;

    wait;
  end process;
end behav;
