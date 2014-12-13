library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;



entity psl_test_working is
end entity psl_test_working;


architecture test of psl_test_working is


  signal s_rst_n : std_logic := '0';
  signal s_clk   : std_logic := '0';
  signal s_write : std_logic;
  signal s_read  : std_logic;


begin


  s_rst_n <= '1' after 100 ns;
  s_clk   <= not s_clk after 10 ns;


  TestP : process is
  begin
    report "RUNNING PSL_TEST_WORKING test case";
    report "==================================";
    s_write <= '0';
    s_read  <= '0';
    wait until s_rst_n = '1' and rising_edge(s_clk);
    s_write <= '1';  -- cover should hit
    wait until rising_edge(s_clk);
    s_read  <= '1';  -- assertion should hit
    wait until rising_edge(s_clk);
    s_write <= '0';
    s_read  <= '0';
    wait;
  end process TestP;



  -- -psl statements

  -- psl default clock is rising_edge(s_clk);

  -- this one works:
  -- psl assert always (s_write -> not(s_read));


end architecture test;
