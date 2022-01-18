entity tb_sub03 is
end tb_sub03;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_sub03 is
  signal i, o : std_logic_vector(3 downto 0) := x"0";
begin
  dut: entity work.sub03
    port map (i => i, o => o);

  process
  begin
    i <= x"0";
    wait for 1 ns;
    assert o = x"8" severity failure;

    i <= x"8";
    wait for 1 ns;
    assert o = x"0" severity failure;

    i <= x"9";
    wait for 1 ns;
    assert o = x"1" severity failure;

    wait;
  end process;
end behav;
