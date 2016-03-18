library ieee;
use ieee.std_logic_1164.all;


library ieee;
use ieee.numeric_std.all;

entity extend_mask is
	port (
		clk : in  std_logic;
		ra0_addr : in  std_logic_vector(4 downto 0);
		ra0_data : out std_logic_vector(20 downto 0)
	);
end extend_mask;
architecture augh of extend_mask is

	-- Embedded RAM

	type ram_type is array (0 to 19) of std_logic_vector(20 downto 0);
	signal ram : ram_type := (
		"111111111111111111110", "111111111111111111100", "111111111111111111000", "111111111111111110000",
		"111111111111111100000", "111111111111111000000", "111111111111110000000", "111111111111100000000",
		"111111111111000000000", "111111111110000000000", "111111111100000000000", "111111111000000000000",
		"111111110000000000000", "111111100000000000000", "111111000000000000000", "111110000000000000000",
		"111100000000000000000", "111000000000000000000", "110000000000000000000", "100000000000000000000"
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

	ra0_data <= ram( to_integer(ra0_addr) ) when to_integer(ra0_addr) < 20 else (others => '-');

end architecture;
