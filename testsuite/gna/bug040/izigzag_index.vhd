library ieee;
use ieee.std_logic_1164.all;


library ieee;
use ieee.numeric_std.all;

entity izigzag_index is
	port (
		clk : in  std_logic;
		ra0_addr : in  std_logic_vector(5 downto 0);
		ra0_data : out std_logic_vector(5 downto 0)
	);
end izigzag_index;
architecture augh of izigzag_index is

	-- Embedded RAM

	type ram_type is array (0 to 63) of std_logic_vector(5 downto 0);
	signal ram : ram_type := (
		"000000", "000001", "001000", "010000", "001001", "000010", "000011", "001010", "010001", "011000", "100000", "011001",
		"010010", "001011", "000100", "000101", "001100", "010011", "011010", "100001", "101000", "110000", "101001", "100010",
		"011011", "010100", "001101", "000110", "000111", "001110", "010101", "011100", "100011", "101010", "110001", "111000",
		"111001", "110010", "101011", "100100", "011101", "010110", "001111", "010111", "011110", "100101", "101100", "110011",
		"111010", "111011", "110100", "101101", "100110", "011111", "100111", "101110", "110101", "111100", "111101", "110110",
		"101111", "110111", "111110", "111111"
	);


	-- Little utility functions to make VHDL syntactically correct
	--   with the syntax to_integer(unsigned(vector)) when 'vector' is a std_logic.
	--   This happens when accessing arrays with <= 2 cells, for example.

	function to_integer(B: std_logic) return integer is
		variable V: std_logic_vector(0 to 0);
	begin
		V(0) := B;
		return to_integer(unsigned(V));
	end;

	function to_integer(V: std_logic_vector) return integer is
	begin
		return to_integer(unsigned(V));
	end;

begin

	-- The component is a ROM.
	-- There is no Write side.

	-- The Read side (the outputs)

	ra0_data <= ram( to_integer(ra0_addr) );

end architecture;
