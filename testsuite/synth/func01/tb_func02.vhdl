entity tb_func02 is
end tb_func02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_func02 is
  signal a, b : std_logic_vector(7 downto 0);
begin
  dut: entity work.func02
    port map (a, b);

  process
  begin
    a <= x"5d";
    wait for 1 ns;
    assert b = x"01" severity failure;

    a <= x"ff";
    wait for 1 ns;
    assert b = x"01" severity failure;

    a <= x"fe";
    wait for 1 ns;
    assert b = x"00" severity failure;

    wait;
  end process;
end behav;
