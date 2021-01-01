entity tb_bug4 is
end tb_bug4;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_bug4 is
  signal clk : std_logic;
  signal o : std_logic;
begin
  dut: entity work.bug4
    port map (clk, o);

  process
  begin
    clk <= '0';
    wait for 1 ns;
    assert o = '1' severity failure;
    clk <= '1';
    wait for 1 ns;
    assert o = '0' severity failure;
    wait;
  end process;
end behav;
