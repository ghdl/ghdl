entity tb_asgn01 is
end tb_asgn01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_asgn01 is
  signal a  : std_logic_vector (2 downto 0);
  signal s0 : std_logic;
  signal r  : std_logic_vector (2 downto 0);
begin
  dut: entity work.asgn01
    port map (a => a, s0 => s0, r => r);

  process
  begin
    s0 <= '1';
    wait for 1 ns;
    assert r = "000" severity failure;

    a <= "101";
    s0 <= '0';
    wait for 1 ns;
    assert r = "101" severity failure;

    a <= "110";
    s0 <= '0';
    wait for 1 ns;
    assert r = "110" severity failure;

    wait;
  end process;
end behav;
