entity tb_forgen01 is
end tb_forgen01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_forgen01 is
  signal a : std_logic_vector (7 downto 0);
begin
  dut: entity work.forgen01
    port map (a);

  process
  begin
    wait for 1 ns;
    assert a = x"a1" severity failure;
    wait;
  end process;
end behav;
