entity tb_func01 is
end tb_func01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_func01 is
  signal a, b : std_logic_vector(7 downto 0);
  signal sel : std_logic;
begin
  dut: entity work.func01
    port map (a, sel, b);

  process
  begin
    a <= x"5d";
    sel <= '1';
    wait for 1 ns;
    assert b = x"0d" severity failure;

    sel <= '0';
    wait for 1 ns;
    assert b = x"5d" severity failure;

    wait;
  end process;
end behav;
