entity tb_concat01 is
end tb_concat01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_concat01 is
  signal a : std_logic_vector(15 downto 0);
begin
  dut: entity work.concat01
    port map (a);

  process
  begin
    wait for 1 ns;
    assert a = x"ab9e" severity failure;

    wait;
  end process;
end behav;
