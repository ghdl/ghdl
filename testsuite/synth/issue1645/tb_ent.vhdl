entity tb_ent is
end tb_ent;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ent is
  signal clk : std_logic;
  signal r : std_logic;
begin
  dut: entity work.ent
    port map (
      clk => clk,
      r => r);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    wait for 1 ns;
    assert r = '1' severity failure;
    pulse;
    assert r = '1' severity failure;
    pulse;
    assert r = '1' severity failure;
    pulse;
    assert r = '0' severity failure;
    pulse;
    assert r = '1' severity failure;
    wait;
  end process;
end behav;
