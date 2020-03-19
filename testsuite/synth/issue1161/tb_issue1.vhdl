entity tb_issue1 is
end tb_issue1;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_issue1 is
  signal a : std_logic_vector (3 downto 0);
begin
  dut: entity work.issue1
    port map (a);

  process
  begin
    wait for 1 ns;
    assert a = "0111" severity failure;
    wait;
  end process;
end behav;
