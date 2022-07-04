library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity bug is
	port (
		a : in unsigned(8-1 downto 0);
		b : in unsigned(8-1 downto 0);
		d : out signed(8 downto 0)
	);
end bug;

architecture behav of bug is
begin
	d <= (abs(signed('0' & a) - signed('0' & b)));
end behav;
