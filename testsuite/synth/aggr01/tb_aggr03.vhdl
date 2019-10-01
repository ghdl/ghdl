entity tb_aggr03 is
end tb_aggr03;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_aggr03 is
  signal a, b : std_logic_vector(7 downto 0);
begin
  dut: entity work.aggr03
    port map (a, b);

  process
  begin
    a <= x"ff";
    wait for 1 ns;
    assert b = x"ff" severity failure;

    a <= x"ee";
    wait for 1 ns;
    assert b = x"ef" severity failure;

    a <= x"50";
    wait for 1 ns;
    assert b = x"53" severity failure;

    wait;
  end process;
end behav;
