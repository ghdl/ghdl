entity tb_pragma01_sim is
end tb_pragma01_sim;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_pragma01_sim is
  signal is_sim : std_logic;
begin
  dut: entity work.pragma01
    port map (is_sim);

  process
  begin
    wait for 1 ns;
    assert is_sim = '1' severity failure;
    wait;
  end process;
end behav;
