library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_ram01 is
end entity;

architecture behav of tb_ram01 is
  signal clk, we : std_logic := '0';
  signal wa, ra : std_logic_vector(1 downto 0) := "00";
  signal d, q : std_logic_vector(7 downto 0) := x"00";
  procedure tick (signal c : out std_logic) is
  begin
    wait for 1 ns; c <= '1'; wait for 1 ns; c <= '0';
  end procedure;
begin
  dut : entity work.ram01 port map (clk, we, wa, ra, d, q);

  process
  begin
    we <= '1'; wa <= "00"; d <= x"11"; tick (clk);
    wa <= "01"; ra <= "01"; d <= x"a5"; tick (clk);
    wait for 1 ns;
    assert q = x"a5" severity failure;
    we <= '0'; ra <= "00"; tick (clk);
    wait for 1 ns;
    assert q = x"11" severity failure;
    wait;
  end process;
end architecture;
