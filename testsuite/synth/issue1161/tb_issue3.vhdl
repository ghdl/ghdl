entity tb_issue3 is
end tb_issue3;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_issue3 is
  signal a : std_logic_vector (3 downto 0);
begin
  dut: entity work.issue3
    port map (a);

  process
  begin
    assert a = "0110" severity failure;
    wait;
  end process;
end behav;
