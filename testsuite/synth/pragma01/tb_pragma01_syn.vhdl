entity tb_pragma01_syn is
end tb_pragma01_syn;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_pragma01_syn is
  signal is_sim : std_logic;
begin
  dut: entity work.pragma01
    port map (is_sim);

  process
  begin
    wait for 1 ns;
    assert is_sim = '0' severity failure;
    wait;
  end process;
end behav;
