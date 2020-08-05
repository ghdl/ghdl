library ieee;
use ieee.std_logic_1164.all,
    ieee.numeric_std.all;

entity test_entity is
	generic (
		ARR_SIZE : natural := 4;
		VECTOR_SIZE : natural := 7
	);
end test_entity;

architecture behaviour of test_entity is
	type type_simple_array is array(natural range <>) of std_logic_vector(7 downto 0);
	type type_multi_array is array(natural range <>) of std_logic_vector;

	signal sig_simple : type_simple_array(0 to ARR_SIZE);
	signal sig_multi_1 : type_multi_array(0 to 4)(7 downto 0);
	signal sig_multi_2 : type_multi_array(0 to 4)(VECTOR_SIZE downto 0);

	-- erroring instruction below
	signal sig_multi_3 : type_multi_array(0 to ARR_SIZE)(7 downto 0);
begin
end behaviour;
