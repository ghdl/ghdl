library ieee;
use ieee.std_logic_1164.all;

entity test_entity is
	generic (
		n : natural := 5;
		p : natural := 8
	);
end test_entity;

architecture behaviour of test_entity is
	type t is array(natural range <>) of std_logic_vector;

	-- all of these work
	signal s_test1: t(0 to 5-1)(p-1 downto 0);
	signal s_test2: t(0 to 5-1)(8-1 downto 0);
	signal s_test3: t(0 to n-1)(p-1 downto 0);

	-- erroring instruction below
	signal s_test4: t(0 to n-1)(8-1 downto 0);
begin
end behaviour;
