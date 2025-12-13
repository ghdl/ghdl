entity tb_smax03 is
end tb_smax03;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_smax03 is
  signal r, res : signed(3 downto 0);
  signal l : integer;
begin
  max01_1: entity work.smax03
    port map (
      a  => l,
      b  => r,
      o => res);

  process
  begin
    l <= 2;
    r <= "0111";
    wait for 1 ns;
    assert res = "0111" severity failure;

    l <= 6;
    r <= "1011";
    wait for 1 ns;
    assert res = "0110" severity failure;

    wait;
  end process;
end behav;
