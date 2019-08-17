entity tb_repro_rng1 is
end tb_repro_rng1;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_repro_rng1 is
  signal clk : std_logic;
  signal a : natural range 0 to 7;
  signal b : natural range 0 to 7;
begin
  dut: entity work.repro_rng1
    port map (
      clk => clk, a => a, b => b);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    a <= 1;
    pulse;
    assert b = 1 severity failure;

    a <= 6;
    pulse;
    assert b = 6 severity failure;
    wait;
  end process;
end behav;
