entity tb_bug is
end tb_bug;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_bug is
  signal clk : std_logic;
  signal o : std_logic;
begin
  dut: entity work.bug
    port map (clk, o);

  process
  begin
    clk <= '0';
    wait for 1 ns;
    assert o = 'U' severity failure;
    clk <= '1';
    wait for 1 ns;
    assert o = '0' severity failure;
    wait;
  end process;
end behav;
