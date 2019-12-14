entity tb_asgn06 is
end tb_asgn06;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_asgn06 is
  signal s0 : std_logic;
  signal clk : std_logic;
  signal r  : std_logic_vector (65 downto 0);
begin
  dut: entity work.asgn06
    port map (clk => clk, s0 => s0, r => r);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    s0 <= '0';
    pulse;
    assert r (0) = '1' severity failure;
    assert r (64 downto 1) = x"ffff_eeee_dddd_cccc" severity failure;
    assert r (65) = '1' severity failure;

    s0 <= '1';
    pulse;
    assert r (0) = '0' severity failure;
    assert r (64 downto 1) = x"ffff_eeee_dddd_cc9c" severity failure;
    assert r (65) = '0' severity failure;

    wait;
  end process;
end behav;
