library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity top is
	port(
		sel : in unsigned(1 downto 0);
		data : in std_logic_vector(3 downto 0);
		q : out std_logic
	);
end entity;

architecture arch of top is
	type record_t is record
		x : std_logic_vector(1 downto 0);
		y : std_logic_vector(1 downto 0);
	end record;

	type array_t is array (0 to 1) of record_t;
	signal a : array_t;
begin
	a <= (("11", data(1 downto 0)), ("11", data(3 downto 2)));
	q <= a(to_integer(sel(1 downto 1))).y(to_integer(sel(0 downto 0)));
end architecture;

--  A0             A1
--    Y1 Y0 X1 X0      Y1 Y0 X1 X0
--  A + sel1*4 + 2 + sel0*1
