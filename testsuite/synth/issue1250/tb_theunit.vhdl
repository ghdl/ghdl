entity tb_theunit is
end tb_theunit;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_theunit is
  signal d : std_logic;
begin
  dut: entity work.theunit
    port map (d);

  process
  begin
    wait for 1 ns;
    assert d = '1' severity failure;
    
    wait;
  end process;
end behav;
