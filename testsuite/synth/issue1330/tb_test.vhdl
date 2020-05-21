entity tb_test is
end tb_test;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_test is
  signal clk : std_logic;
  signal wr : std_logic;
begin
  dut: entity work.test
    port map (clk, wr);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    wr <= '0';
    pulse;

    pulse;

    wait;
  end process;
end behav;
