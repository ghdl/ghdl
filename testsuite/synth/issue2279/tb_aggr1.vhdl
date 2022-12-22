entity tb_aggr1 is
end tb_aggr1;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_aggr1 is
  signal a : std_logic_vector(7 downto 0);
  signal b : std_logic;
  signal c : std_logic_vector(8 downto 0);
begin
  dut: entity work.aggr1
    port map (a, b, c);

  process
  begin
    c <= b"1_1100_0011";
    wait for 1 ns;
    assert a = x"e1" severity failure;
    assert b = '1' severity failure;

    c <= b"0_1100_0100";
    wait for 1 ns;
    assert a = x"62" severity failure;
    assert b = '0' severity failure;

    wait;
  end process;
end behav;
