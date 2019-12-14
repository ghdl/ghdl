entity tb_ent is
end tb_ent;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ent is
  signal clk : std_logic;
  signal counter : natural;
  signal rst : std_logic;
begin
  dut: entity work.ent
    port map (
      rst => rst,
      clk => clk,
      counter => counter);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    rst <= '1';
    pulse;
    assert counter = 0 severity failure;
    rst <= '0';
    pulse;
    assert counter = 1 severity failure;
    pulse;
    assert counter = 2 severity failure;
    pulse;
    assert counter = 3 severity failure;
    wait;
  end process;
end behav;
