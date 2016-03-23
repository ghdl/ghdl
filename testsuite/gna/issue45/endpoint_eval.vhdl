library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library std;
use std.env.all;



entity psl_endpoint_eval_in_vhdl is
end entity psl_endpoint_eval_in_vhdl;



architecture test of psl_endpoint_eval_in_vhdl is


  signal s_rst_n : std_logic := '0';
  signal s_clk   : std_logic := '0';
  signal s_write : std_logic;
  signal s_read  : std_logic;


begin


  s_rst_n <= '1' after 100 ns;
  s_clk   <= not s_clk after 10 ns;


  TestP : process is
  begin
    report "RUNNING psl_endpoint_eval_in_vhdl test case";
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
    wait until rising_edge(s_clk);
    stop(0);
    wait;
  end process TestP;


  -- psl default clock is rising_edge(s_clk);
  -- psl endpoint E_TEST0 is {not(s_write); s_write};


  process is
  begin
    wait until E_TEST0;
    report "HIT";
    wait;
  end process;


end architecture test;
