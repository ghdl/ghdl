entity tb_repro_nat is
end tb_repro_nat;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_repro_nat is
  signal clk : std_logic;
  signal a : natural;
  signal b : natural;
begin
  dut: entity work.repro_nat
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
    a <= 125;
    pulse;
    assert b = 125 severity failure;

    a <= 7689;
    pulse;
    assert b = 7689 severity failure;
    wait;
  end process;
end behav;
