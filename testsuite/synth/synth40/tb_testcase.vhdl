entity tb_testcase is
end tb_testcase;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_testcase is
  signal di : std_logic;
  signal do : std_logic;
begin
  dut: entity work.testcase
    port map (data_in => di, data_out => do);

  process
  begin
    di <= '1';
    wait for 1 ns;
    assert do = '0' severity failure;
    
    di <= '0';
    wait for 1 ns;
    assert do = '1' severity failure;
    
    wait;
  end process;
end behav;
