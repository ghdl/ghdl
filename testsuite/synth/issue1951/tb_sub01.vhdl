entity tb_sub01 is
end tb_sub01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_sub01 is
  signal i, o : std_logic_vector(3 downto 0) := x"0";
begin
  dut: entity work.sub01
    port map (i => i, o => o);

  process
  begin
    i <= x"0";
    wait for 1 ns;
    assert o = x"1" severity failure;
    wait;
  end process;
end behav;
