library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;



library work;
use work.all;


entity	test_harness is

end entity;

architecture behv of test_harness is
signal clk    :std_logic := '0';
signal reset  :std_logic := '0';

begin

clk <= not clk after 10 ns;

reset	<= '0' after 100 ns;




end architecture;

