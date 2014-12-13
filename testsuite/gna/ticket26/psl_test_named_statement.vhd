library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;



entity psl_test_named_statement is
end entity psl_test_named_statement;


architecture test of psl_test_named_statement is


  signal s_rst_n : std_logic := '0';
  signal s_clk   : std_logic := '0';
  signal s_write : std_logic;
  signal s_read  : std_logic;


begin


  s_rst_n <= '1' after 100 ns;
  s_clk   <= not s_clk after 10 ns;


  TestP : process is
  begin
    report "RUNNING psl_test_named_statement test case";
    report "==========================================";
    s_write <= '0';  -- named assertion should hit
    s_read  <= '0';
    wait until s_rst_n = '1' and rising_edge(s_clk);
    s_write <= '1';
    wait until rising_edge(s_clk);
    s_read  <= '1';  -- assertion should hit
    wait until rising_edge(s_clk);
    s_write <= '0';
    s_read  <= '0';
    wait;
  end process TestP;


  -- -psl statements

  -- psl default clock is rising_edge(s_clk);

  -- named statements seems to be not supported (ignored by GHDL)
  -- psl RESET_CHECK : assert always not(s_rst_n) -> s_write;

  -- statements without name work
  -- - psl assert always s_write -> not(s_read);


end architecture test;
