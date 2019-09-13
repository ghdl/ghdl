entity tb_func04 is
end tb_func04;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_func04 is
  signal a, b, r : std_logic_vector(7 downto 0);
begin
  dut: entity work.func04
    port map (a, b, r);

  process
  begin
    a <= x"5d";
    b <= x"78";
    wait for 1 ns;
    assert r = x"79" severity failure;

    a <= x"0f";
    b <= x"f0";
    wait for 1 ns;
    assert r = x"f3" severity failure;

    wait;
  end process;
end behav;
