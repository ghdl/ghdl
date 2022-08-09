library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity bug is
	port(
		a :  in unsigned( 7 downto 0);
		b :  in unsigned(15 downto 0);
		r : out unsigned(23 downto 0)
	);
end entity;

architecture rtl of bug is

begin
	r <= resize(a*b, 24);
end architecture;
