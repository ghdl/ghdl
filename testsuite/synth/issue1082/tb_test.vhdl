entity tb_test is
end tb_test;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_test is
  signal a : std_logic_vector (31 downto 0) := (others => '0');
  signal b : std_logic_vector (31 downto 0);
begin
  dut: entity work.test
    port map (a_in => a, b_out => b);

  process
  begin
    a <= x"0000_0003";
    wait for 1 ns;
    assert b = x"0000_0007" severity failure;
    wait;
  end process;
end behav;
