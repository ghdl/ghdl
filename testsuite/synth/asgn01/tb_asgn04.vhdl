entity tb_asgn04 is
end tb_asgn04;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_asgn04 is
  signal s0 : std_logic;
  signal s1 : std_logic;
  signal r  : std_logic_vector (2 downto 0);
begin
  dut: entity work.asgn04
    port map (s0 => s0, s1 => s1, r => r);

  process
  begin
    s0 <= '0';
    s1 <= '0';
    wait for 1 ns;
    assert r = "000" severity failure;

    s0 <= '0';
    s1 <= '1';
    wait for 1 ns;
    assert r = "000" severity failure;

    s0 <= '1';
    s1 <= '0';
    wait for 1 ns;
    assert r = "010" severity failure;

    s0 <= '1';
    s1 <= '1';
    wait for 1 ns;
    assert r = "001" severity failure;

    wait;
  end process;
end behav;
