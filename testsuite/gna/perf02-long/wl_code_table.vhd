library ieee;
use ieee.std_logic_1164.all;


library ieee;
use ieee.numeric_std.all;

entity wl_code_table is
	port (
		clk : in  std_logic;
		ra0_data : out std_logic_vector(31 downto 0);
		ra0_addr : in  std_logic_vector(3 downto 0)
	);
end wl_code_table;
architecture augh of wl_code_table is

	-- Embedded RAM

	type ram_type is array (0 to 15) of std_logic_vector(31 downto 0);
	signal ram : ram_type := ("11111111111111111111111111000100", "00000000000000000000101111100010", "00000000000000000000010010101110", "00000000000000000000001000011010", "00000000000000000000000101001110", "00000000000000000000000010101100", "00000000000000000000000000111010", "11111111111111111111111111100010", "00000000000000000000101111100010", "00000000000000000000010010101110", "00000000000000000000001000011010", "00000000000000000000000101001110", "00000000000000000000000010101100", "00000000000000000000000000111010", "11111111111111111111111111100010", "11111111111111111111111111000100");


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
