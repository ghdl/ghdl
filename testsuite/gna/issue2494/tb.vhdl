library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.all;
use work.compa_pkg.all;


entity tb is
begin
end entity tb;

architecture behv of tb is

signal clk	:std_logic := '0';
signal rst	:std_logic := '1';


begin

clk <= not clk after 10 ns;
rst	<= '0' after 100 ns;

top_i : entity top
	port map (
		clk => clk,
		rst => rst
	);

TST: process (all)
begin
	test_record_proc(<<signal.tb.top_i.sigb:test_rec(a(11 downto 0), b(11 downto 0))>>.a);
end process;


end architecture behv;
