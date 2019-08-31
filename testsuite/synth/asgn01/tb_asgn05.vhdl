entity tb_asgn05 is
end tb_asgn05;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_asgn05 is
  signal s0 : std_logic;
  signal s1 : std_logic;
  signal r  : std_logic_vector (5 downto 0);
begin
  dut: entity work.asgn05
    port map (s0 => s0, s1 => s1, r => r);

  process
  begin
    s0 <= '0';
    s1 <= '0';
    wait for 1 ns;
    assert r = "000000" severity failure;

    s0 <= '1';
    s1 <= '0';
    wait for 1 ns;
    assert r = "010110" severity failure;

    wait;
  end process;
end behav;
