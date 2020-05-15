entity tb_issue is
end tb_issue;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_issue is
  signal a : boolean;
begin
  dut: entity work.issue
    port map (a);

  process
  begin
    wait for 1 ns;
    assert a severity failure;
    wait;
  end process;
end behav;
