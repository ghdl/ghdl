entity tb_test5 is
end tb_test5;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_test5 is
  signal r : std_logic_vector(7 downto 0);
begin
  dut: entity work.test5
    port map (r);

  process
  begin
    wait for 1 ns;
    assert r(7) = '1' severity failure;
    wait;
  end process;
end behav;
