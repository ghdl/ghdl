library ieee;
use ieee.std_logic_1164.all;


library ieee;
use ieee.numeric_std.all;

entity zigzag_index is
	port (
		clk : in  std_logic;
		ra0_addr : in  std_logic_vector(5 downto 0);
		ra0_data : out std_logic_vector(5 downto 0)
	);
end zigzag_index;
architecture augh of zigzag_index is

	-- Embedded RAM

	type ram_type is array (0 to 63) of std_logic_vector(5 downto 0);
	signal ram : ram_type := (
		"000000", "000001", "000101", "000110", "001110", "001111", "011011", "011100", "000010", "000100", "000111", "001101",
		"010000", "011010", "011101", "101010", "000011", "001000", "001100", "010001", "011001", "011110", "101001", "101011",
		"001001", "001011", "010010", "011000", "011111", "101000", "101100", "110101", "001010", "010011", "010111", "100000",
		"100111", "101101", "110100", "110110", "010100", "010110", "100001", "100110", "101110", "110011", "110111", "111100",
		"010101", "100010", "100101", "101111", "110010", "111000", "111011", "111101", "100011", "100100", "110000", "110001",
		"111001", "111010", "111110", "111111"
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
