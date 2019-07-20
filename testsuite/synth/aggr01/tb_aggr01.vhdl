entity tb_aggr01 is
end tb_aggr01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_aggr01 is
  signal a, b : std_logic_vector(7 downto 0);
begin
  dut: entity work.aggr01
    port map (a, b);

  process
  begin
    a <= x"ff";
    wait for 1 ns;
    assert b = x"01" severity failure;

    a <= x"ee";
    wait for 1 ns;
    assert b = x"00" severity failure;

    wait;
  end process;
end behav;
