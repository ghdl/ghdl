entity tb_bug2 is
end tb_bug2;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_bug2 is
  signal clk : std_logic;
  signal o : std_logic_vector(3 downto 0);
begin
  dut: entity work.bug2
    port map (clk, o);

  process
  begin
    clk <= '0';
    wait for 1 ns;
    assert o = "UUUU" severity failure;
    clk <= '1';
    wait for 1 ns;
    assert o = "0110" severity failure;
    wait;
  end process;
end behav;
