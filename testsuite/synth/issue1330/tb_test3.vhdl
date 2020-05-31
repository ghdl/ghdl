entity tb_test3 is
end tb_test3;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_test3 is
  signal clk : std_logic;
  signal wr : std_logic;
  signal arst : std_logic;
begin
  dut: entity work.test3
    port map (clk, wr, arst);

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
    arst <= '1';
    pulse;

    report "cycle 2";
    arst <= '0';
    pulse;

    report "cycle 3";
    
    arst <= '1';
    wr <= '1';
    pulse;

    wait;
  end process;
end behav;
