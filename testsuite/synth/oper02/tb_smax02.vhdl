entity tb_smax02 is
end tb_smax02;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_smax02 is
  signal l, res : signed(3 downto 0);
  signal r : integer;
begin
  max01_1: entity work.smax02
    port map (
      a  => l,
      b  => r,
      o => res);

  process
  begin
    l <= "0111";
    r <= 2;
    wait for 1 ns;
    assert res = "0111" severity failure;

    l <= "1011";
    r <= 6;
    wait for 1 ns;
    assert res = "0110" severity failure;

    wait;
  end process;
end behav;
