entity tb_revrng01 is
end tb_revrng01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_revrng01 is
  signal a, z : std_logic_vector (7 downto 0);
begin
  dut: entity work.revrng01
    port map (a, z);

  process
  begin
    a <= x"a1";
    wait for 1 ns;
    assert z = x"85" severity failure;
    wait;
  end process;
end behav;
