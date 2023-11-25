library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;



library work;
use work.all;


entity tb is
begin
end entity tb;

architecture behv of tb is

alias clk 				is 	<<signal.tb.th.clk:std_logic>>;
signal clk_tb	:std_logic;

begin
clk_tb <= clk;

th : entity work.test_harness;

--p_main: process

--begin

--clk_tb <= '0';

--end process;


end architecture behv;
