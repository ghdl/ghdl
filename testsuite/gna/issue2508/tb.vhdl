library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.all;
--use work.compa_pkg.all;


entity tb is
begin
end entity tb;

architecture behv of tb is

signal rst	:std_logic := '1';
alias tb_clk 				is 	<<signal.tb.th.clk:std_logic>>;

begin

rst	<= '0' after 100 ns;

top_i : entity top;

TST: process (all)
begin


end process;


end architecture behv;
