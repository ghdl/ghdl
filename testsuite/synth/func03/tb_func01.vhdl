entity tb_func01 is
end tb_func01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_func01 is
  signal a, b : std_logic_vector(7 downto 0);
begin
  dut: entity work.func01
    port map (a, b);

  process
  begin
    a <= x"5d";
    wait for 1 ns;
    assert b = x"ba" severity failure;

    a <= x"ff";
    wait for 1 ns;
    assert b = x"fe" severity failure;

    a <= x"23";
    wait for 1 ns;
    assert b = x"46" severity failure;

    wait;
  end process;
end behav;
