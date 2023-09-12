library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.all;


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

end architecture behv;
