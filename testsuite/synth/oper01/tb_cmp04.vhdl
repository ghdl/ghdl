entity tb_cmp04 is
end tb_cmp04;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_cmp04 is
  signal l  : std_logic_vector(3 downto 0);
  signal res : std_logic;
begin
  cmp01_1: entity work.cmp04
    port map (
      l  => l,
      res  => res);

  process
  begin
    l <= x"5";
    wait for 1 ns;
    assert res = '0' severity failure;

    l <= x"0";
    wait for 1 ns;
    assert res = '0' severity failure;

    wait;
  end process;
end behav;
