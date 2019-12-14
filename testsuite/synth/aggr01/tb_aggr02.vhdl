entity tb_aggr02 is
end tb_aggr02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_aggr02 is
  signal a, b : std_logic_vector(7 downto 0);
begin
  dut: entity work.aggr02
    port map (a, b);

  process
  begin
    a <= x"ff";
    wait for 1 ns;
    assert b = x"fe" severity failure;

    a <= x"ee";
    wait for 1 ns;
    assert b = x"ee" severity failure;

    wait;
  end process;
end behav;
