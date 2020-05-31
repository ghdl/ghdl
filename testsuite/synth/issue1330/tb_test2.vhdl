entity tb_test2 is
end tb_test2;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_test2 is
  signal clk : std_logic;
  signal wr : std_logic;
  signal rst : std_logic;
begin
  dut: entity work.test2
    port map (clk, wr, rst);

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
    rst <= '1';
    pulse;

    rst <= '0';
    pulse;

    rst <= '1';
    wr <= '1';
    pulse;
    
    wait;
  end process;
end behav;
