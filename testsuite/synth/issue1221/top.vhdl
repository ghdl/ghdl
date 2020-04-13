library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity top is
	port(
		a : in unsigned(7 downto 0);
		q : out unsigned(7 downto 0)
	);
end entity;

architecture arch of top is
begin
	q <= 0 - a;
end architecture;
