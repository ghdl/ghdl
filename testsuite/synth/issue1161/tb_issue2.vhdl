entity tb_issue2 is
end tb_issue2;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_issue2 is
  signal a : std_logic_vector (3 downto 0);
begin
  dut: entity work.issue2
    port map (a);

  process
  begin
    wait for 1 ns;
    assert a = "1001" severity failure;
    wait;
  end process;
end behav;
