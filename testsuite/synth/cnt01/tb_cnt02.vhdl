entity tb_cnt02 is
end tb_cnt02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_cnt02 is
  signal clk : std_logic;
  signal rst : std_logic;
  signal low : std_logic;
begin
  dut: entity work.cnt02
    port map (clk => clk, rst => rst, low => low);
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
    assert low = '0' severity failure;

    rst <= '0';
    pulse;
    assert low = '0' severity failure;

    pulse;
    assert low = '0' severity failure;

    pulse;
    assert low = '0' severity failure;

    pulse;
    assert low = '1' severity failure;

    pulse;
    assert low = '1' severity failure;


    wait;
  end process;
end behav;
